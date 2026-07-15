{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.Metadata.Webhook.Types where

import Data.Aeson ( FromJSON, ToJSON, parseJSON, toJSON, (.:), (.:?) )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.List.NonEmpty ( NonEmpty )
import qualified Data.List.NonEmpty as NE
import Data.Text ( Text )
import qualified Data.Text as T
import Servant
import qualified Servant.GitHub.Webhook as SGH

import qualified Cardano.Metadata.Types.Weakly as Weakly

newtype GitHubKey = GitHubKey (forall result. SGH.GitHubKey result)

gitHubKey :: IO BS.ByteString -> GitHubKey
gitHubKey k = GitHubKey (SGH.gitHubKey k)

instance HasContextEntry '[GitHubKey] (SGH.GitHubKey result) where
    getContextEntry (GitHubKey x :. _) = x

newtype GitHubToken = GitHubToken { ghToken :: Text }
  deriving (Eq, Show)

-- | Normalize the raw METADATA_GITHUB_TOKEN environment variable, treating
-- both an unset and an empty-string value as "no token" (anonymous GitHub
-- API access) rather than as a bad token to send. Unlike the webhook
-- secret, an absent GitHub token is a legitimate configuration -- public
-- repositories don't need one -- so this never fails closed.
resolveGithubToken :: Maybe String -> Maybe GitHubToken
resolveGithubToken Nothing = Nothing
resolveGithubToken (Just s)
  | null s    = Nothing
  | otherwise = Just (GitHubToken (T.pack s))

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
  toJSON (PushEvent' headCommit repoInfo) = Aeson.object
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
    Aeson.object
      [ ("added"    , toJSON $ maybe [] NE.toList added)
      , ("modified" , toJSON $ maybe [] NE.toList modified)
      , ("removed"  , toJSON $ maybe [] NE.toList removed)
      ]

-- | Only the repository's full name ("owner/repo") is trusted from the
-- webhook payload, and only to check it against the server-configured
-- 'GitHubRepo' -- never to build a URL. The payload previously also carried
-- a "contents_url" that was used directly to build the GitHub API request,
-- letting a forged (or otherwise attacker-influenced) payload point that
-- request at an arbitrary host with our GitHub token attached.
data RepositoryInfo
  = RepositoryInfo { repoInfoFullName :: Text
                   }
  deriving (Eq, Show)

instance FromJSON RepositoryInfo where
  parseJSON = Aeson.withObject "RepositoryInfo" $ \obj ->
    RepositoryInfo
    <$> obj .: "full_name"

instance ToJSON RepositoryInfo where
  toJSON (RepositoryInfo fullName) = Aeson.object
    [("full_name", toJSON fullName)]

-- | A server-side pinned GitHub repository. Constructed only from
-- operator-supplied configuration (CLI/env), never from webhook payload
-- data, so it can be used to build GitHub API request URLs without
-- exposing our GitHub token to a host an attacker gets to choose.
data GitHubRepo
  = GitHubRepo { ghRepoOwner :: Text
               , ghRepoName  :: Text
               }
  deriving (Eq, Show)

-- | The full name ("owner/repo") of a pinned 'GitHubRepo', in the same
-- format as GitHub's "full_name" field, for comparison against
-- 'repoInfoFullName'.
ghRepoFullName :: GitHubRepo -> Text
ghRepoFullName (GitHubRepo owner name) = owner <> "/" <> name

-- | The GitHub REST "contents" API URL prefix for fetching files from a
-- pinned repository, e.g. "https://api.github.com/repos/owner/repo/contents/".
ghRepoContentsUrl :: GitHubRepo -> Text
ghRepoContentsUrl repo =
  "https://api.github.com/repos/" <> ghRepoFullName repo <> "/contents/"

type GetEntryFromFile = RepositoryInfo -> Text -> IO (Maybe Weakly.Metadata)
