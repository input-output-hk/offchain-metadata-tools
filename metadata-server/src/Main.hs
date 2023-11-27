{-# OPTIONS_HADDOCK show-extensions #-}

{- |
  Module – Module name
  Description – Short description
  Maintainer – samuel.evans-powell@iohk.io
  Stability – experimental
-}

module Main where

import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan ( newChan )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Logger
    ( LogLevel (LevelDebug)
    , filterLogger
    , runChanLoggingT
    , runStdoutLoggingT
    , unChanLoggingT
    )
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Database.Persist.Postgresql as Postgresql
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative as Opt
import qualified Text.Regex as R

import Cardano.Metadata.Server ( webApp )
import qualified Cardano.Metadata.Store.Postgres as Store
import Cardano.Metadata.Store.Postgres.Config ( Opts (..), pgConnectionString )
import Config ( opts )

main :: IO ()
main = do
  options@(Opts { optDbConnections = numDbConns
                , optDbMetadataTableName = tableName
                , optServerPort = port
                }) <- Opt.execParser opts

  let pgConnString = pgConnectionString options
  putStrLn . obfuscatePasswords $ "Connecting to database using connection string: " <> BC.unpack pgConnString

  runStdoutLoggingT $ filterLogger (\_ lvl -> lvl /= LevelDebug) $ do
    -- Create log channel
    logChan <- liftIO newChan

    -- Spawn server
    _ <- liftIO $ forkIO $ runChanLoggingT logChan $ Postgresql.withPostgresqlPool pgConnString numDbConns $ \pool -> liftIO $ do
      putStrLn $ "Initializing table '" <> tableName <> "'."
      intf <- Store.postgresStore pool (T.pack tableName)

      putStrLn $ "Metadata server is starting on port " <> show port <> "."
      liftIO $ Warp.run port (webApp intf)

    -- Logging in main thread
    unChanLoggingT logChan

obfuscatePasswords :: String -> String
obfuscatePasswords clear = R.subRegex (R.mkRegex "password=\\S+") clear "password=*******"
