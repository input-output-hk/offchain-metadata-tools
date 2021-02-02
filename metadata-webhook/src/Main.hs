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
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Control.Lens ((^.), (.~))
import Data.Function ((&))
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as C8
import           Data.Maybe                   ( fromJust )
import           GitHub.Data.Webhooks.Events  ( PullRequestEvent(..), IssueCommentEvent(..), PushEvent(..))
import qualified Database.Persist.Postgresql as Postgresql
import           GitHub.Data.Webhooks.Payload ( HookIssueComment(..), HookUser(..) )
import           Network.Wai                  ( getRequestBodyChunk, Request, Application )
import           Network.HTTP.Types.Status (Status, ok200)
import           Network.HTTP.Types (hAccept, hUserAgent, hAuthorization)
import Network.HTTP.Client (requestBody)
import           Network.Wai.Handler.Warp     ( runSettings, defaultSettings, setPort, setLogger)
import           System.Environment           ( lookupEnv )
import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON, (.:), (.:?))
import Control.Monad.Logger (runStdoutLoggingT)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Servant
import qualified Servant.GitHub.Webhook       as SGH
import           Servant.GitHub.Webhook       ( GitHubEvent, GitHubSignedReqBody, RepoWebhookEvent(..) )
import qualified Network.Wreq as Wreq
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative as Opt

import           Cardano.Metadata.Webhook.Types
import           Cardano.Metadata.Webhook.Server
import           Cardano.Metadata.Server.Types (Subject, Entry', _eSubject)
import           Cardano.Metadata.Store.Types (StoreInterface(..))
import           Cardano.Metadata.Store.Postgres.Config (Opts(..), pgConnectionString)
import qualified Cardano.Metadata.Store.Postgres as Store
import Config (opts)

main :: IO ()
main = do
  key         <- maybe mempty C8.pack <$> lookupEnv "METADATA_WEBHOOK_SECRET"
  putStrLn "1"
  githubToken <- GitHubToken <$> maybe "" read <$> lookupEnv "METADATA_GITHUB_TOKEN"
  putStrLn "2"

  options@(Opts { optDbConnections       = numDbConns
                , optDbMetadataTableName = tableName
                , optServerPort          = port
                }) <- Opt.execParser opts
  putStrLn "3"

  let pgConnString = pgConnectionString options
  putStrLn $ "Connecting to database using connection string: " <> C8.unpack pgConnString
  runStdoutLoggingT $
    Postgresql.withPostgresqlPool pgConnString numDbConns $ \pool -> liftIO $ do
      putStrLn $ "Initializing table '" <> tableName <> "'."
      intf <- Store.postgresStore pool (T.pack tableName)
      
      putStrLn $ "Metadata webhook is starting on port " <> show port <> "."
      liftIO $ Warp.run port (appSigned (gitHubKey $ pure key) intf (getFileContent githubToken))
