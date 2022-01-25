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
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty ( NonEmpty )
import qualified Data.List.NonEmpty as NE
import Data.Text ( Text )
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
