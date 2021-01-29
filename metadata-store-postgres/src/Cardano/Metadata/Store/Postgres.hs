{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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

import           Prelude hiding (init, read)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Exception.Safe
import           Data.Coerce (coerce)
import           Database.Persist hiding (update, delete)
import           Data.Traversable (for)
import           Control.Monad.Reader
import          Data.Text (Text)
import           Database.Persist.Sql (SqlBackend, ConnectionPool, Single(Single))
import qualified Database.Persist.Sql as Sql
import qualified Database.Persist.Postgresql as Postgresql
import           Control.Monad.Logger (logInfoN, runNoLoggingT, runStderrLoggingT,
                     runStdoutLoggingT)
import Cardano.Metadata.Store.Types 
import Data.Pool
import Data.Word
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Data.Aeson (ToJSONKey, ToJSON, ToJSONKeyFunction, FromJSON, FromJSONKey)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Encoding.Internal as Aeson

data PostgresKeyValueException = UniqueKeyConstraintViolated
                               | FailedToDecodeJSONValue String Text
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
  :: ( ToJSONKey k
     , ToJSON v
     )
  => ConnectionPool
  -- ^ Database connection pool
  -> Text
  -- ^ Database table name
  -> IO (KeyValue k v)
  -- ^ Resulting key-value store
init pool tableName =
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
                        (\k v -> write k v kvs)
                        (\k   -> delete k kvs)
                        (\f k -> update f k kvs)
                        (toList kvs)
                        (empty kvs)

m :: IO (KeyValue Word8 Aeson.Value)
m =
  runStdoutLoggingT $
    Postgresql.withPostgresqlPool "host=/run/postgresql dbname=cexplorer user=cardano-node" 1 $ \pool -> liftIO $ do
      kvs <- init pool "record"
      result <- read 1 kvs
      write 2 (Aeson.Object $ HM.fromList [("name", Aeson.Number 3)]) kvs
      xs <- toList kvs
      traverse_ (putStrLn . show) xs
      empty kvs
      ys <- toList kvs
      putStrLn $ show ys
      pure kvs

withBackendFromPool :: Pool r -> ReaderT r IO b -> IO b
withBackendFromPool pool action = withResource pool (runReaderT action)

read :: (ToJSONKey k, FromJSON v) => k -> KeyValue k v -> IO (Maybe v)
read k (KeyValue pool tableName) = do
  results <- withBackendFromPool pool $
    Sql.rawSql
      ("SELECT value FROM " <> tableName <> " WHERE \"key\" = ?")
      [toPersistValueJSONKey k]
  case results of
    []            -> pure Nothing
    (Single x):[] -> handleJSONDecodeError x $ decodeJSONValue x
    xs            -> throw UniqueKeyConstraintViolated

write :: (ToJSONKey k, ToJSON v) => k -> v -> KeyValue k v -> IO ()
write k v (KeyValue pool tableName) = withBackendFromPool pool $ do
  Sql.rawExecute
    ("INSERT INTO " <> tableName <> " (key, value) VALUES (?, ?) ON CONFLICT (key) DO UPDATE SET value = EXCLUDED.value")
    [ toPersistValueJSONKey k
    , toPersistValueJSON v
    ]

delete :: ToJSONKey k => k -> KeyValue k v -> IO ()
delete k (KeyValue pool tableName) = withBackendFromPool pool $ do
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
  results <- withBackendFromPool pool $
    Sql.rawSql
      ("SELECT key, value FROM " <> tableName)
      []
  for results $ \(Single kText, Single vText) -> do
    k <- handleJSONDecodeError kText $ decodeJSONKey kText
    v <- handleJSONDecodeError vText $ decodeJSONValue vText
    pure (k, v)

empty :: KeyValue k v -> IO ()
empty (KeyValue pool tableName) = withBackendFromPool pool $
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
handleJSONDecodeError t = either (\err -> throw $ FailedToDecodeJSONValue err t) pure
      
toPersistValueJSONKey :: ToJSONKey k => k -> PersistValue
toPersistValueJSONKey k =
  let
    k' =
      case Aeson.toJSONKey of
        Aeson.ToJSONKeyText  f _ -> f k
        Aeson.ToJSONKeyValue _ f -> TL.toStrict $ TLE.decodeUtf8 $ Aeson.encodingToLazyByteString $ f k
  in
    toPersistValue k'
