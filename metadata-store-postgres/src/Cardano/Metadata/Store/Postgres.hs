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
  ) where

import Cardano.Metadata.Store.Types
import Control.Exception.Safe
import Control.Monad ( unless )
import Control.Monad.IO.Class ( MonadIO )
import Control.Monad.Reader ( ReaderT, runReaderT )
import Data.Aeson ( FromJSON, FromJSONKey, ToJSON, ToJSONKey )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding.Internal as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as Aeson
import Data.Char ( isAsciiLower, isAsciiUpper, isDigit )
import Data.Coerce ( coerce )
import qualified Data.Map.Strict as M
import Data.Pool
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Traversable ( for )
import Database.Persist hiding ( delete, update )
import Database.Persist.Sql ( ConnectionPool, Single (Single), SqlBackend )
import qualified Database.Persist.Sql as Sql
import Prelude hiding ( init, read )

data PostgresKeyValueException = UniqueKeyConstraintViolated
                               | FailedToDecodeJSONValue String Text
                               | InvalidTableName Text
  deriving (Eq, Show, Exception)

data KeyValue k v = KeyValue { _kvConnPool    :: Pool SqlBackend
                             , _kvDbTableName :: Text
                             }

createTable
  :: ( BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Text
  -- ^ Table name
  -> ReaderT backend m ()
createTable tableName =
  Sql.rawExecute ("CREATE TABLE IF NOT EXISTS " <> tableName <> "(\"key\" VARCHAR PRIMARY KEY UNIQUE, \"value\" JSONB NOT NULL)") []

init
  :: ConnectionPool
  -- ^ Database connection pool
  -> Text
  -- ^ Database table name
  -> IO (KeyValue k v)
  -- ^ Resulting key-value store
init pool tableName = do
  checkTableName tableName

  withBackendFromPool pool $ do
    createTable tableName

    pure $ KeyValue pool tableName

postgresStore
  :: ( ToJSONKey k
     , ToJSON v
     , FromJSONKey k
     , FromJSON v
     )
  => ConnectionPool
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

withBackendFromPool :: Pool r -> ReaderT r IO b -> IO b
withBackendFromPool pool action = withResource pool (runReaderT action)

read :: (ToJSONKey k, FromJSON v) => k -> KeyValue k v -> IO (Maybe v)
read k (KeyValue pool tableName) = do
  checkTableName tableName

  results <- withBackendFromPool pool $
    Sql.rawSql
      ("SELECT value FROM " <> tableName <> " WHERE \"key\" = ?")
      [toPersistValueJSONKey k]
  case results of
    []            -> pure Nothing
    (Single x):[] -> handleJSONDecodeError x $ decodeJSONValue x
    _xs           -> throwIO UniqueKeyConstraintViolated

readBatch :: (ToJSONKey k, FromJSON v) => [k] -> KeyValue k v -> IO [v]
readBatch [] (KeyValue _pool tableName) = checkTableName tableName >> pure []
readBatch ks (KeyValue pool tableName) = do
  checkTableName tableName

  results <- fmap (fmap (\(k, v) -> (Sql.unSingle k, Sql.unSingle v))) $  withBackendFromPool pool $
    Sql.rawSql
      ("SELECT key, value FROM " <> tableName <> " WHERE \"key\" IN (" <> placeholders <> ")")
      (map toPersistValueJSONKey ks)
  resultMap <- flip foldMap results $ \(k, v) -> do
    v' <- handleJSONDecodeError v $ decodeJSONValue v
    pure $ M.singleton k v'

  pure $ flip foldMap ks $ \k ->
    case M.lookup (toJSONKeyText k) resultMap of
      Nothing -> []
      Just v  -> [v]

  where
    placeholders = T.intercalate "," (map (const "?") ks)

write :: (ToJSONKey k, ToJSON v) => k -> v -> KeyValue k v -> IO ()
write k v (KeyValue pool tableName) = do
  checkTableName tableName

  withBackendFromPool pool $ do
    Sql.rawExecute
      ("INSERT INTO " <> tableName <> " (key, value) VALUES (?, ?) ON CONFLICT (key) DO UPDATE SET value = EXCLUDED.value")
      [ toPersistValueJSONKey k
      , toPersistValueJSON v
      ]

delete :: ToJSONKey k => k -> KeyValue k v -> IO ()
delete k (KeyValue pool tableName) = do
  checkTableName tableName

  withBackendFromPool pool $ do
    Sql.rawExecute
      ("DELETE FROM " <> tableName <> " WHERE \"key\" = ?")
      [toPersistValueJSONKey k]

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
  checkTableName tableName

  results <- withBackendFromPool pool $
    Sql.rawSql
      ("SELECT key, value FROM " <> tableName)
      []
  for results $ \(Single kText, Single vText) -> do
    k <- handleJSONDecodeError kText $ decodeJSONKey kText
    v <- handleJSONDecodeError vText $ decodeJSONValue vText
    pure (k, v)

empty :: KeyValue k v -> IO ()
empty (KeyValue pool tableName) = do
  checkTableName tableName

  withBackendFromPool pool $
    Sql.rawExecute ("TRUNCATE " <> tableName) []

decodeJSONValue :: FromJSON v => Text -> Either String v
decodeJSONValue = Aeson.eitherDecode . TLE.encodeUtf8 . TL.fromStrict

decodeJSONKey :: FromJSONKey k => Text -> Either String k
decodeJSONKey t = case Aeson.fromJSONKey of
  Aeson.FromJSONKeyCoerce -> pure $ coerce t
  Aeson.FromJSONKeyText f -> pure $ f t
  Aeson.FromJSONKeyTextParser p -> Aeson.parseEither p t
  Aeson.FromJSONKeyValue pv -> do
    (v :: Aeson.Value) <- Aeson.eitherDecode (TLE.encodeUtf8 . TL.fromStrict $ t)
    Aeson.parseEither pv v

handleJSONDecodeError :: Text -> Either String a -> IO a
handleJSONDecodeError t = either (\err -> throwIO $ FailedToDecodeJSONValue err t) pure

toPersistValueJSONKey :: ToJSONKey k => k -> PersistValue
toPersistValueJSONKey = toPersistValue . toJSONKeyText

toJSONKeyText :: ToJSONKey k => k -> Text
toJSONKeyText k =
  case Aeson.toJSONKey of
    Aeson.ToJSONKeyText  f _ -> Key.toText (f k)
    Aeson.ToJSONKeyValue _ f -> TL.toStrict $ TLE.decodeUtf8 $ Aeson.encodingToLazyByteString $ f k

-- | The table name is spliced directly into raw SQL statements (Postgres
-- does not support binding identifiers as query parameters), so it is
-- restricted to a safe subset of SQL identifier characters to rule out SQL
-- injection via a malicious @--db-table@ value.
isSafeTableName :: Text -> Bool
isSafeTableName t =
  not (T.null t)
    && not (isDigit (T.head t))
    && T.all isSafeChar t
  where
    isSafeChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'

-- | Every function in this module that splices a table name into a raw SQL
-- statement calls this first, rather than relying solely on the check in
-- 'init', so that validation doesn't silently lapse if a 'KeyValue' is ever
-- constructed some other way in future.
checkTableName :: Text -> IO ()
checkTableName tableName =
  unless (isSafeTableName tableName) $ throwIO (InvalidTableName tableName)
