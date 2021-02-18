{-# LANGUAGE OverloadedStrings #-}

module Cardano.Metadata.Webhook.Server where

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

import           Cardano.Metadata.Server.Types (Subject(Subject), Entry', _eSubject)
import           Cardano.Metadata.Webhook.Types 
import           Cardano.Metadata.Webhook.API
import           Cardano.Metadata.Store.Types (StoreInterface(..))


appSigned :: GitHubKey -> StoreInterface Subject Entry' -> GetEntryFromFile -> Application
appSigned key intf getEntryFromFile
  = serveWithContext
    (Proxy :: Proxy MetadataWebhookAPISigned)
    (key :. EmptyContext)
    (metadataWebhook intf getEntryFromFile)

appUnsigned :: StoreInterface Subject Entry' -> GetEntryFromFile -> Application
appUnsigned intf getEntryFromFile
  = serveWithContext
    (Proxy :: Proxy MetadataWebhookAPIUnsigned)
    EmptyContext
    (metadataWebhookUnsigned intf getEntryFromFile)

metadataWebhook :: StoreInterface Subject Entry' -> GetEntryFromFile -> Server MetadataWebhookAPISigned
metadataWebhook intf getEntryFromFile = (\a (_, b) -> pushHook intf getEntryFromFile a b)

metadataWebhookUnsigned :: StoreInterface Subject Entry' -> GetEntryFromFile -> Server MetadataWebhookAPIUnsigned
metadataWebhookUnsigned = pushHook

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
pushHook :: StoreInterface Subject Entry' -> GetEntryFromFile -> RepoWebhookEvent -> PushEvent' -> Handler ()
pushHook intf getEntryFromFile _ ev@(PushEvent' (Commit added modified removed) repoInfo) = liftIO $ do
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
        Just subject -> delete (Subject subject)
