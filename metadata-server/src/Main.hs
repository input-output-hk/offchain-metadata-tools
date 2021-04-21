{-# OPTIONS_HADDOCK show-extensions #-}

{- |
  Module – Module name
  Description – Short description
  Maintainer – samuel.evans-powell@iohk.io
  Stability – experimental
-}

module Main where

import Control.Monad.IO.Class
    ( liftIO )
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative as Opt
import Control.Exception (bracket)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close)
import Data.Pool (Pool, createPool, destroyAllResources)
import Data.Time.Clock (NominalDiffTime)

import Cardano.Metadata.Server
    ( webApp )
import qualified Cardano.Metadata.Store.Postgres as Store
import Cardano.Metadata.Store.Postgres.Config
    ( Opts (..), pgConnectionString )
import Config
    ( opts )

main :: IO ()
main = do
  options@(Opts { optDbConnections = numDbConns
                , optDbMetadataTableName = tableName
                , optServerPort = port
                }) <- Opt.execParser opts

  let pgConnString = pgConnectionString options
  putStrLn $ "Connecting to database using connection string: " <> BC.unpack pgConnString
  Store.withConnectionPool pgConnString numDbConns $ \pool -> do
    putStrLn $ "Initializing table '" <> tableName <> "'."
    intf <- Store.postgresStore pool (T.pack tableName)

    putStrLn $ "Metadata server is starting on port " <> show port <> "."
    liftIO $ Warp.run port (webApp intf)
