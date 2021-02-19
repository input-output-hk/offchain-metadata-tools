{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Cardano.Metadata.Webhook.Types where

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
import           GitHub.Data.Webhooks.Payload ( HookIssueComment(..), HookUser(..) )
import           Network.Wai                  ( getRequestBodyChunk, Request, Application )
import           Network.HTTP.Types.Status (Status, ok200)
import           Network.HTTP.Types (hAccept, hUserAgent, hAuthorization)
import Network.HTTP.Client (requestBody)
import           Network.Wai.Handler.Warp     ( runSettings, defaultSettings, setPort, setLogger)
import           System.Environment           ( lookupEnv )
import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON, (.:), (.:?))
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

import           Cardano.Metadata.Types.Common (Subject)
import qualified Cardano.Metadata.Types.Weakly as Weakly
import           Cardano.Metadata.Store.Types (StoreInterface(..))

newtype GitHubKey = GitHubKey (forall result. SGH.GitHubKey result)

gitHubKey :: IO BS.ByteString -> GitHubKey
gitHubKey k = GitHubKey (SGH.gitHubKey k)

instance HasContextEntry '[GitHubKey] (SGH.GitHubKey result) where
    getContextEntry (GitHubKey x :. _) = x

newtype GitHubToken = GitHubToken { ghToken :: Text }
  deriving (Eq, Show)

data PushEvent'
  = PushEvent' { evPushHeadCommit :: Commit
               , evPushRepository :: RepositoryInfo
               }
  deriving (Eq, Show)

instance FromJSON PushEvent' where
  parseJSON = Aeson.withObject "PushEvent'" $ \obj ->
    PushEvent'
    <$> obj .: "head_commit"
    <*> obj .: "repository"

instance ToJSON PushEvent' where
  toJSON (PushEvent' headCommit repoInfo) = Aeson.Object $ HM.fromList $
    [ ("head_commit", toJSON headCommit)
    , ("repository", toJSON repoInfo)
    ]

data Commit
  = Commit { comFilesAdded    :: Maybe (NonEmpty Text)
           , comFilesModified :: Maybe (NonEmpty Text)
           , comFilesRemoved  :: Maybe (NonEmpty Text)
           }
  deriving (Eq, Show)

instance FromJSON Commit where
  parseJSON = Aeson.withObject "Commit" $ \obj ->
    Commit
    <$> ((NE.nonEmpty =<<) <$> obj .:? "added")
    <*> ((NE.nonEmpty =<<) <$> obj .:? "modified")
    <*> ((NE.nonEmpty =<<) <$> obj .:? "removed")

instance ToJSON Commit where
  toJSON (Commit added modified removed) =
    Aeson.Object $ HM.fromList
      [ ("added"    , toJSON $ maybe [] NE.toList added)
      , ("modified" , toJSON $ maybe [] NE.toList modified)
      , ("removed"  , toJSON $ maybe [] NE.toList removed)
      ]

data RepositoryInfo
  = RepositoryInfo { repoInfoContentsUrl :: Text
                   }
  deriving (Eq, Show)

instance FromJSON RepositoryInfo where
  parseJSON = Aeson.withObject "RepositoryInfo" $ \obj ->
    RepositoryInfo
    <$> obj .: "contents_url"

instance ToJSON RepositoryInfo where
  toJSON (RepositoryInfo contentsUrl) = Aeson.Object $ HM.fromList $
    [("contents_url", toJSON contentsUrl)]

type GetEntryFromFile = RepositoryInfo -> Text -> IO (Maybe Weakly.Metadata)
