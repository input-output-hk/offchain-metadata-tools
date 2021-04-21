{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Metadata.Store.Postgres
  ( read
  , write
  , update
  , delete
  , empty
  , toList
  , init
  , postgresStore
  , PostgresKeyValueException(..)
  , mkConnectionPool
  , withConnectionPool
  ) where

import Cardano.Metadata.Store.Types
import Data.Time (NominalDiffTime)
import Control.Exception.Safe
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BC
import Data.Foldable (foldl', foldMap')
import Data.Aeson
    ( FromJSON, FromJSONKey, ToJSON, ToJSONKey )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding.Internal as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Coerce
    ( coerce )
import qualified Data.Map.Strict as M
import Data.Pool
import Data.Text
    ( Text )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Traversable
    ( for )
import Database.PostgreSQL.Simple (Connection, ConnectInfo(..), In(..), query, Only(..))
import Database.PostgreSQL.Simple.Types (Identifier(..), Binary(..))
import qualified Database.PostgreSQL.Simple as Sql
import Prelude hiding
    ( init, read )

data PostgresKeyValueException = UniqueKeyConstraintViolated
                               | FailedToDecodeJSONValue String Text
  deriving (Eq, Show, Exception)

data KeyValue k v = KeyValue { _kvConnPool    :: Pool Connection
                             , _kvDbTableName :: Text
                             }

mkConnectionPool
  :: BC.ByteString
  -- ^ Libpq connection string
  -> Int
  -- ^ Maximum number of postgresql connections to allow
  -> IO (Pool Connection)
mkConnectionPool connectionStr numConns =
  createPool
    (Sql.connectPostgreSQL connectionStr)
    Sql.close
    1                       -- Number of sub-pools
    (10 :: NominalDiffTime) -- Amount of time for which an unused connection is kept open
    numConns

withConnectionPool
  :: BC.ByteString
  -- ^ Libpq connection string
  -> Int
  -- ^ Maximum number of postgresql connections to allow
  -> (Pool Connection -> IO r)
  -> IO r
withConnectionPool connectionInfo numConns f = bracket
  (mkConnectionPool connectionInfo numConns)
  destroyAllResources
  f

createTable
  :: Connection
  -- ^ Connection to database
  -> Text
  -- ^ Table name
  -> IO ()
createTable conn tableName =
  void $ Sql.execute conn ("CREATE TABLE IF NOT EXISTS ? (\"key\" VARCHAR PRIMARY KEY UNIQUE, \"value\" JSONB NOT NULL)") (Only $ Identifier tableName)

init
  :: Pool Connection
  -- ^ Database connection pool
  -> Text
  -- ^ Database table name
  -> IO (KeyValue k v)
  -- ^ Resulting key-value store
init pool tableName =
  withConnectionFromPool pool $ \conn -> do
    createTable conn tableName

    pure $ KeyValue pool tableName

postgresStore
  :: ( ToJSONKey k
     , ToJSON v
     , FromJSONKey k
     , FromJSON v
     )
  => Pool Connection
  -- ^ Database connection pool
  -> Text
  -- ^ Database table name
  -> IO (StoreInterface k v)
postgresStore pool tableName = do
  kvs <- init pool tableName
  pure $ StoreInterface (\k   -> read k kvs)
                        (\ks  -> readBatch ks kvs)
                        (\k v -> write k v kvs)
                        (\k   -> delete k kvs)
                        (\f k -> update f k kvs)
                        (toList kvs)
                        (empty kvs)

withConnectionFromPool :: Pool Connection -> (Connection -> IO b) -> IO b
withConnectionFromPool pool action = withResource pool $ action

read :: (ToJSONKey k, FromJSON v) => k -> KeyValue k v -> IO (Maybe v)
read k (KeyValue pool tableName) = do
  (results :: [Only Aeson.Value]) <- withConnectionFromPool pool $ \conn ->
    Sql.query conn ("SELECT value FROM ? WHERE \"key\" = ?") (Identifier tableName, toJSONKeyText k)
  case results of
    []            -> pure Nothing
    (Only v):[]   -> fromValue v
    _vs           -> throw UniqueKeyConstraintViolated

readBatch :: (ToJSONKey k, FromJSON v) => [k] -> KeyValue k v -> IO [v]
readBatch [] (KeyValue _pool _tableName) = pure []
readBatch ks (KeyValue pool tableName) = do
  (results :: [(Text, Aeson.Value)]) <- withConnectionFromPool pool $ \conn ->
    Sql.query conn
      ("SELECT key, value FROM ? WHERE \"key\" IN ?")
      (Identifier tableName, In $ toJSONKeyText <$> ks)

  resultMap <- flip foldMap' results $ \(k, v) -> do
    v' <- fromValue v
    pure $ M.singleton k v'

  pure $ flip foldMap' ks $ \k ->
    case M.lookup (toJSONKeyText k) resultMap of
      Nothing -> []
      Just v  -> [v]

write :: (ToJSONKey k, ToJSON v) => k -> v -> KeyValue k v -> IO ()
write k v (KeyValue pool tableName) = withConnectionFromPool pool $ \conn ->
  void $ Sql.execute conn
    ("INSERT INTO ? (key, value) VALUES (?, ?) ON CONFLICT (key) DO UPDATE SET value = EXCLUDED.value")
    (Identifier tableName, toJSONKeyText k, Aeson.toJSON v)

delete :: ToJSONKey k => k -> KeyValue k v -> IO ()
delete k (KeyValue pool tableName) = withConnectionFromPool pool $ \conn ->
  void $ Sql.execute conn
    ("DELETE FROM ? WHERE \"key\" = ?")
    (Identifier tableName, toJSONKeyText k)

update :: (ToJSONKey k, ToJSON v, FromJSON v) => (v -> Maybe v) -> k -> KeyValue k v -> IO ()
update fv k kvs = do
  mv <- read k kvs
  case mv of
    Nothing -> pure ()
    Just v  -> case fv v of
      Nothing       -> delete k kvs
      Just newValue -> write k newValue kvs

toList :: (FromJSONKey k, FromJSON v) => KeyValue k v -> IO [(k, v)]
toList (KeyValue pool tableName) = do
  (results :: [(Text, Aeson.Value)]) <- withConnectionFromPool pool $ \conn ->
    Sql.query conn
      ("SELECT key, value FROM ?")
      (Only $ Identifier tableName)
  for results $ \(kText, v) -> do
    k <- handleJSONDecodeError (Aeson.String kText) $ decodeJSONKey kText
    v <- fromValue v
    pure (k, v)

empty :: KeyValue k v -> IO ()
empty (KeyValue pool tableName) = withConnectionFromPool pool $ \conn ->
  void $ Sql.execute conn ("TRUNCATE ?") (Only $ Identifier tableName)

decodeJSONKey :: FromJSONKey k => Text -> Either String k
decodeJSONKey t = case Aeson.fromJSONKey of
  Aeson.FromJSONKeyCoerce -> pure $ coerce t
  Aeson.FromJSONKeyText f -> pure $ f t
  Aeson.FromJSONKeyTextParser p -> Aeson.parseEither p t
  Aeson.FromJSONKeyValue pv -> do
    (v :: Aeson.Value) <- Aeson.eitherDecode (TLE.encodeUtf8 . TL.fromStrict $ t)
    Aeson.parseEither pv v

handleJSONDecodeError :: Aeson.Value -> Either String a -> IO a
handleJSONDecodeError v = either (\err -> throw $ FailedToDecodeJSONValue err (TL.toStrict $ Aeson.encodeToLazyText v)) pure

toJSONKeyText :: ToJSONKey k => k -> Text
toJSONKeyText k =
  case Aeson.toJSONKey of
    Aeson.ToJSONKeyText  f _ -> f k
    Aeson.ToJSONKeyValue _ f -> TL.toStrict $ TLE.decodeUtf8 $ Aeson.encodingToLazyByteString $ f k

fromValue :: Aeson.FromJSON a => Aeson.Value -> IO a
fromValue v = handleJSONDecodeError v $ Aeson.eitherDecode $ Aeson.encode v
