{-# OPTIONS_HADDOCK show-extensions #-}

{- |
  Module – Module name
  Description – Short description
  Maintainer – samuel.evans-powell@iohk.io
  Stability – experimental
-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import qualified Options.Applicative as Opt
import qualified Database.Persist.Postgresql as Postgresql
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

import Cardano.Metadata.Server (webApp)
import qualified Cardano.Metadata.Store.Postgres as Store
import Cardano.Metadata.Store.Postgres.Config (pgConnectionString, Opts(..))
import Config (opts)

main :: IO ()
main = do
  options@(Opts { optDbConnections = numDbConns
                , optDbMetadataTableName = tableName
                , optServerPort = port
                }) <- Opt.execParser opts

  let pgConnString = pgConnectionString options
  putStrLn $ "Connecting to database using connection string: " <> BC.unpack pgConnString
  runStdoutLoggingT $
    Postgresql.withPostgresqlPool pgConnString numDbConns $ \pool -> liftIO $ do
      putStrLn $ "Initializing table '" <> tableName <> "'."
      intf <- Store.postgresStore pool (T.pack tableName)
      
      putStrLn $ "Metadata server is starting on port " <> show port <> "."
      liftIO $ Warp.run port (webApp intf)
