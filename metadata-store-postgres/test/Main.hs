{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.IO.Class
    ( liftIO )
import qualified Data.Pool as Pool
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
import qualified Database.PostgreSQL.Simple as Postgresql
import Test.Tasty
    ( TestTree
    , askOption
    , defaultIngredients
    , defaultMainWithIngredients
    , includingOptions
    , testGroup
    , withResource
    )
import Test.Tasty.Options

import Cardano.Metadata.Store.Postgres
import Cardano.Metadata.Store.Types
import Test.Cardano.Metadata.Generators
    ( ComplexType )
import Test.Cardano.Metadata.Store

newtype DbHost = DbHost { _dbHost :: Text }
newtype DbName = DbName { _dbName :: Text }
newtype DbUser = DbUser { _dbUser :: Text }

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

main :: IO ()
main =
    defaultMainWithIngredients
      ((includingOptions
          [ Option (Proxy @DbHost)
          , Option (Proxy @DbName)
          , Option (Proxy @DbUser)
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
    withResource
      (do
         pool <- mkConnectionPool (TE.encodeUtf8 $ "host=" <> dbHost <> " dbname=" <> dbName <> " user=" <> dbUser) 1
         postgresIntf1 <- liftIO $ postgresStore pool "record"
         postgresIntf2 <- liftIO $ postgresStore pool "record"

         pure (pool, postgresIntf1, postgresIntf2)
      )
      (\(pool, _, _) -> Pool.destroyAllResources pool)
      tests

tests
  :: IO ( Pool.Pool Postgresql.Connection
        , StoreInterface Word8 Word8
        , StoreInterface Text ComplexType
        )
  -> TestTree
tests resources =
  testGroup "integration tests"
    [
      testGroup "Postgres data store implementation"
       [ testKeyValueImplementation ((\(_, intf, _) -> intf) <$> resources)
       , testKeyValueComplexTypeImplementation ((\(_, _, intf) -> intf) <$> resources)
       ]
    ]
