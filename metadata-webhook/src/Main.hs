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

import           Cardano.Metadata.Server.Types (Subject, Entry', _eSubject)
import           Cardano.Metadata.Store.Types (StoreInterface(..))

-- Lib
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

-- API defn.
type MetadataWebhookAPISigned   = "webhook" :> PushHookAPIF GitHubSignedReqBody
type MetadataWebhookAPIUnsigned = "webhook" :> PushHookAPIF ReqBody

type PushHookAPIF reqBody
  =  GitHubEvent '[ 'WebhookPushEvent ]
  :> reqBody '[JSON] PushEvent'
  :> Post '[JSON] ()

type GetEntryFromFile = RepositoryInfo -> Text -> IO (Maybe Entry')

getFileContent :: GitHubToken -> GetEntryFromFile
getFileContent (GitHubToken githubToken) repoInfo fileName = do
  let
    rawContentsUrl     = repoInfoContentsUrl repoInfo
    substitutionSuffix = "{+path}"
  fileContentsUrl <- case T.stripSuffix substitutionSuffix rawContentsUrl of
    Nothing  -> error $ "Repository contents URL '" <> T.unpack rawContentsUrl <> "' is missing '" <> T.unpack substitutionSuffix <> "' substitution suffix."
    Just url -> pure $ url <> fileName

  let options = Wreq.defaults & Wreq.header hAccept .~ ["application/vnd.github.v3.raw"]
                              & Wreq.header hUserAgent .~ ["metadata-webhook"]
                              & Wreq.header hAuthorization .~ [C8.pack . T.unpack $ githubToken]
                              & Wreq.checkResponse .~ (pure mempty)

  resp <- Wreq.getWith options (T.unpack fileContentsUrl)

  case resp ^. Wreq.responseStatus of
    s | s /= ok200 -> do
      putStrLn $ "Failed to get file contents from URL: '" <> T.unpack fileContentsUrl <> "'."
      pure Nothing
    _              ->
      let
        rawContents = resp ^. Wreq.responseBody
      in
        case Aeson.eitherDecode rawContents of
          Left err    -> do
            putStrLn $ "Failed to decode contents of file '" <> T.unpack fileName <> "' into an Entry', error was: '" <> err <> "', contents of file were: '" <> BLC.unpack rawContents <> "'."
            pure Nothing
          Right entry -> pure (Just entry)
  

-- API handlers
pushHook :: StoreInterface Subject Entry' -> GetEntryFromFile -> RepoWebhookEvent -> ((), PushEvent') -> Handler ()
pushHook intf getEntryFromFile _ (_, ev@(PushEvent' (Commit added modified removed) repoInfo)) = liftIO $ do
  -- putStrLn $ (show . whUserLogin . evPullRequestSender) ev ++ " pullRequested a commit, resulting in the following event: "
  putStrLn $ BLC.unpack $ Aeson.encode ev

  withFirstFile added    (writeEntry intf)
  withFirstFile modified (writeEntry intf)
  withFirstFile removed  (removeEntry intf)

  where
    withFirstFile :: Monoid m => Maybe (NonEmpty Text) -> (Text -> m) -> m
    withFirstFile Nothing _         = mempty
    withFirstFile (Just (x :| _)) f = f x

    writeEntry :: StoreInterface Subject Entry' -> Text -> IO ()
    writeEntry (StoreInterface { storeWrite = write }) file = do
      mEntry <- getEntryFromFile repoInfo file
      case mEntry of
        Nothing    -> pure ()
        Just entry -> do
          let subject = _eSubject entry
          write subject entry

    removeEntry :: StoreInterface Subject Entry' -> Text -> IO ()
    removeEntry (StoreInterface { storeDelete = delete }) removedFile = do
      let jsonSuffix = ".json"
      case T.stripSuffix jsonSuffix removedFile of
        Nothing      -> putStrLn $ "Not removing 'removedFile' because it's file extension does not match: '" <> T.unpack jsonSuffix <> "'."
        Just subject -> delete subject


metadataWebhook :: StoreInterface Subject Entry' -> GetEntryFromFile -> Server MetadataWebhookAPISigned
metadataWebhook = pushHook

main :: IO ()
main = do
  port        <- maybe 8080 read <$> lookupEnv "METADATA_WEBHOOK_PORT"
  key         <- maybe mempty C8.pack <$> lookupEnv "METADATA_WEBHOOK_SECRET"
  githubToken <- GitHubToken <$> maybe "" read <$> lookupEnv "METADATA_GITHUB_TOKEN"

  mainF port key githubToken

mainF :: Warp.Port -> C8.ByteString -> GitHubToken -> IO ()
mainF port key githubToken = do
  putStrLn $ "Server is starting on port " ++ show port ++ " using test secret " ++ show key
  putStrLn $ "Perhaps run 'ngrok http " ++ show port ++ "' for a forwarding address"

  let intf             =
        StoreInterface
          undefined
          undefined 
          (\k v -> putStrLn $ "Wrote '" <> show k <> "': '" <> show v <> "'.")
          (\k   -> putStrLn $ "Deleted '" <> show k <> ".")
          undefined
          undefined
          undefined
      getEntryFromFile = getFileContent githubToken

  runSettings
    ( defaultSettings
     & setLogger logger
     & setPort port
    )
    (app (gitHubKey $ pure key) intf getEntryFromFile)

logger :: Request -> Status -> Maybe Integer -> IO ()
logger req status _fileSize = do
  putStrLn $ "Received request: " <> show req <> ", status: " <> show status <> "."
  body <- getRequestBodyChunk req
  putStrLn $ "Request body: " <> show body

app :: GitHubKey -> StoreInterface Subject Entry' -> GetEntryFromFile -> Application
app key intf getEntryFromFile
  = serveWithContext
    (Proxy :: Proxy MetadataWebhookAPISigned)
    (key :. EmptyContext)
    (metadataWebhook intf getEntryFromFile)

-- TODO Take "write" and "read" functions as arguments to a function, expose that function
-- Allows users of this library to easily choose their DB backend
-- We'll use psql database backend here.

-- TODO Ensure file size < 400kb
