{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception
    ( SomeException, catch )
import Control.Monad
    ( join, void )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Logger
    ( runNoLoggingT )
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import Data.Pool
    ( Pool, destroyAllResources )
import Data.Proxy
    ( Proxy (Proxy) )
import Data.Tagged
    ( Tagged, unTagged )
import Data.Text
    ( Text )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word
    ( Word8 )
import Hedgehog
    ( MonadGen, evalIO, forAll, property, (===) )
import qualified Hedgehog as H
    ( Property )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
    ( TestTree
    , askOption
    , defaultIngredients
    , defaultMainWithIngredients
    , includingOptions
    , testGroup
    , withResource
    )
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Options

import Cardano.Metadata.Store.Types
import Cardano.Metadata.Sync
    ( write )
import Cardano.Metadata.Sync.Config
    ( mkConnectionPool, withConnectionFromPool )
import Cardano.Metadata.Types.Common
    ( Subject (..), unPropertyName )
import Database.PostgreSQL.Simple
    ( Connection, execute, executeMany, query, withTransaction )
import Database.PostgreSQL.Simple.Types
    ( Identifier (..), Only (..) )
import Test.Cardano.Metadata.Generators
    ( ComplexType )
import qualified Test.Cardano.Metadata.Generators as Gen
import Test.Cardano.Metadata.Store

newtype DbHost = DbHost { _dbHost :: Text }
newtype DbName = DbName { _dbName :: Text }
newtype DbUser = DbUser { _dbUser :: Text }
newtype DbTableName = DbTableName { _dbTableName :: Text }

instance IsOption DbHost where
  defaultValue   = DbHost "/run/postgresql"
  parseValue str = Just $ DbHost (T.pack str)
  optionName     = pure "db-host"
  optionHelp     = pure "Postgres server hostname or, for UNIX domain sockets, the socket filename."

instance IsOption DbName where
  defaultValue   = error $ "'" <> unTagged (optionName :: Tagged DbName String) <> "' is required."
  parseValue str = Just $ DbName (T.pack str)
  optionName     = pure "db-name"
  optionHelp     = pure "Database name within the postgres server."

instance IsOption DbUser where
  defaultValue   = error $ "'" <> unTagged (optionName :: Tagged DbUser String) <> "' is required."
  parseValue str = Just $ DbUser (T.pack str)
  optionName     = pure "db-user"
  optionHelp     = pure "User with which to authenticate to the postgres server"

instance IsOption DbTableName where
  defaultValue   = DbTableName "metadata_integration_test"
  parseValue str = Just $ DbTableName (T.pack str)
  optionName     = pure "db-table"
  optionHelp     = pure "Postgres database table to submit test data to."

main :: IO ()
main =
    defaultMainWithIngredients
      ((includingOptions
          [ Option (Proxy @DbHost)
          , Option (Proxy @DbName)
          , Option (Proxy @DbUser)
          , Option (Proxy @DbTableName)
          ]
       )
       : defaultIngredients
      )
      testsWithPosgresConn

testsWithPosgresConn :: TestTree
testsWithPosgresConn =
  askOption $ \(DbHost dbHost) ->
  askOption $ \(DbName dbName) ->
  askOption $ \(DbUser dbUser) ->
  askOption $ \(DbTableName dbTableName) ->
    withResource
      ( (,dbTableName) <$> mkConnectionPool (TE.encodeUtf8 $ "host=" <> dbHost <> " dbname=" <> dbName <> " user=" <> dbUser) 1 )
      ( \(pool, _) -> destroyAllResources pool )
      tests

tests
  :: IO ( Pool Connection, Text )
  -> TestTree
tests resources =
  testGroup "integration tests"
    [
      testGroup "Metadata sync script"
       [ testProperty "write/last-wins" (prop_write_last_wins resources)
       , testProperty "write/no-partial-write" (prop_write_no_partial resources)
       ]
    ]

prop_write_last_wins :: IO (Pool Connection, Text) -> H.Property
prop_write_last_wins getPool = property $ do
  (pool, tableName) <- evalIO getPool
  kvs1 <- forAll genKvs
  kvs2 <- forAll genKvs

  join $ evalIO $ withConnectionFromPool pool $ \conn -> do
    reset conn tableName
    result <- write conn tableName kvs1 >> write conn tableName kvs2 >> toList conn tableName

    reset conn tableName
    expected <- write conn tableName kvs2 >> toList conn tableName

    pure $
      result === expected

prop_write_no_partial :: IO (Pool Connection, Text) -> H.Property
prop_write_no_partial getPool = property $ do
  (pool, tableName) <- evalIO getPool
  kvs <- forAll genKvs

  join $ evalIO $ withConnectionFromPool pool $ \conn -> do
    reset conn tableName
    result <- (write conn tableName (kvs <> [undefined]) `catch` (\(_ :: SomeException) -> pure ())) >> toList conn tableName

    pure $
      result === []

reset :: Connection -> Text -> IO ()
reset conn table = do
  void $ execute conn "CREATE TABLE IF NOT EXISTS ? (\"key\" VARCHAR PRIMARY KEY UNIQUE, \"value\" JSONB NOT NULL)" (Only $ Identifier table)
  void $ execute conn "TRUNCATE ?" (Only $ Identifier table)

toList :: Connection -> Text -> IO [(Subject, Aeson.Value)]
toList conn table = do
  results <- query conn "SELECT key, value FROM ?" (Only $ Identifier table)

  pure $ fmap (\(k, v) -> (Subject k, v)) results 

genKvs :: MonadGen m => m [(Subject, Aeson.Value)]
genKvs =
  let
    kv = (,) <$> Gen.subject <*> (fmap (Aeson.Object . HM.fromList) $ Gen.list (Range.linear 0 20) ((,) <$> (unPropertyName <$> Gen.propertyName) <*> Gen.propertyValue))
  in
    Gen.list (Range.linear 0 15) kv
