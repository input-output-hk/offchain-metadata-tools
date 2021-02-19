{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Main where

import           Control.Monad.IO.Class       ( liftIO )
import qualified Data.Text as T
import qualified Data.ByteString.Char8        as C8
import qualified Database.Persist.Postgresql as Postgresql
import           System.Environment           ( lookupEnv )
import Control.Monad.Logger (runStdoutLoggingT)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative as Opt

import           Cardano.Metadata.Webhook.Types
import           Cardano.Metadata.Webhook.Server
import           Cardano.Metadata.Store.Postgres.Config (Opts(..), pgConnectionString)
import qualified Cardano.Metadata.Store.Postgres as Store
import Config (opts)

main :: IO ()
main = do
  key         <- maybe mempty C8.pack <$> lookupEnv "METADATA_WEBHOOK_SECRET"
  githubToken <- GitHubToken <$> maybe "" T.pack <$> lookupEnv "METADATA_GITHUB_TOKEN"

  options@(Opts { optDbConnections       = numDbConns
                , optDbMetadataTableName = tableName
                , optServerPort          = port
                }) <- Opt.execParser opts

  let pgConnString = pgConnectionString options
  putStrLn $ "Connecting to database using connection string: " <> C8.unpack pgConnString
  runStdoutLoggingT $
    Postgresql.withPostgresqlPool pgConnString numDbConns $ \pool -> liftIO $ do
      putStrLn $ "Initializing table '" <> tableName <> "'."
      intf <- Store.postgresStore pool (T.pack tableName)
      
      putStrLn $ "Metadata webhook is starting on port " <> show port <> "."
      liftIO $ Warp.run port (appSigned (gitHubKey $ pure key) intf (getFileContent githubToken))
