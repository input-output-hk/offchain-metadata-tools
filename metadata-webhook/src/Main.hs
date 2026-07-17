{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Logger ( runStdoutLoggingT )
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Database.Persist.Postgresql as Postgresql
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative as Opt
import System.Environment ( lookupEnv )
import System.Exit ( die )
import qualified Text.Regex as R

import qualified Cardano.Metadata.Store.Postgres as Store
import Cardano.Metadata.Store.Postgres.Config ( Opts (..), pgConnectionString )
import Cardano.Metadata.Webhook.Secret ( resolveWebhookSecret )
import Cardano.Metadata.Webhook.Server
import Cardano.Metadata.Webhook.Types
import Config ( WebhookOpts (..), opts )

main :: IO ()
main = do
  -- Parse arguments first: --help, --version and the shell-completion
  -- scripts are handled inside execParser, which then exits, so those
  -- invocations (including the completion generation done at build time)
  -- must not require the runtime environment. Only a real startup proceeds
  -- past this point, where the webhook secret is then mandatory -- resolved
  -- eagerly here, before any DB connection or the server is started.
  WebhookOpts { optGithubOwner = githubOwner
              , optGithubRepo  = githubRepo
              , optDbOpts      = options@(Opts { optDbConnections       = numDbConns
                                                , optDbMetadataTableName = tableName
                                                , optServerPort          = port
                                                })
              } <- Opt.execParser opts

  secretEnv <- lookupEnv "METADATA_WEBHOOK_SECRET"
  key <- case resolveWebhookSecret secretEnv of
    Left err -> die err
    Right k  -> pure k

  githubToken <- resolveGithubToken <$> lookupEnv "METADATA_GITHUB_TOKEN"
  case githubToken of
    Nothing -> putStrLn "METADATA_GITHUB_TOKEN not set (or empty); GitHub API requests will be made anonymously and are subject to GitHub's public rate limits."
    Just _  -> pure ()

  let githubRepoInfo = GitHubRepo githubOwner githubRepo

  let pgConnString = pgConnectionString options
  putStrLn . obfuscatePasswords $ "Connecting to database using connection string: " <> C8.unpack pgConnString
  runStdoutLoggingT $
    Postgresql.withPostgresqlPool pgConnString numDbConns $ \pool -> liftIO $ do
      putStrLn $ "Initializing table '" <> tableName <> "'."
      intf <- Store.postgresStore pool (T.pack tableName)

      putStrLn $ "Metadata webhook is starting on port " <> show port <> "."
      liftIO $ Warp.run port (appSigned key (gitHubKey $ pure key) intf (getFileContent githubToken githubRepoInfo))

obfuscatePasswords :: String -> String
obfuscatePasswords clear = R.subRegex (R.mkRegex "password=\\S+") clear "password=*******"
