{-# LANGUAGE OverloadedStrings #-}

module Cardano.Metadata.Webhook.Server where

import           Control.Lens                   ((.~), (^.))
import           Control.Monad.IO.Class         (liftIO)
import qualified Data.Aeson                     as Aeson
import qualified Data.ByteString.Char8          as C8
import qualified Data.ByteString.Lazy.Char8     as BLC
import           Data.Foldable                  (traverse_)
import           Data.Function                  ((&))
import           Data.List.NonEmpty             (NonEmpty)
import qualified Data.List.NonEmpty             as NE
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import System.FilePath.Posix (takeBaseName)
import           Network.HTTP.Types             (hAccept, hAuthorization,
                                                 hUserAgent)
import           Network.HTTP.Types.Status      (ok200)
import qualified Network.Wreq                   as Wreq
import           Servant
import           Servant.GitHub.Webhook         (RepoWebhookEvent (..))

import           Cardano.Metadata.Store.Types   (StoreInterface (..))
import           Cardano.Metadata.Types.Common  (Subject (Subject))
import qualified Cardano.Metadata.Types.Weakly  as Weakly
import           Cardano.Metadata.Webhook.API
import           Cardano.Metadata.Webhook.Types


appSigned :: GitHubKey -> StoreInterface Subject Weakly.Metadata -> GetEntryFromFile -> Application
appSigned key intf getEntryFromFile
  = serveWithContext
    (Proxy :: Proxy MetadataWebhookAPISigned)
    (key :. EmptyContext)
    (metadataWebhook intf getEntryFromFile)

appUnsigned :: StoreInterface Subject Weakly.Metadata -> GetEntryFromFile -> Application
appUnsigned intf getEntryFromFile
  = serveWithContext
    (Proxy :: Proxy MetadataWebhookAPIUnsigned)
    EmptyContext
    (metadataWebhookUnsigned intf getEntryFromFile)

metadataWebhook :: StoreInterface Subject Weakly.Metadata -> GetEntryFromFile -> Server MetadataWebhookAPISigned
metadataWebhook intf getEntryFromFile = (\a (_, b) -> pushHook intf getEntryFromFile a b)

metadataWebhookUnsigned :: StoreInterface Subject Weakly.Metadata -> GetEntryFromFile -> Server MetadataWebhookAPIUnsigned
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
pushHook :: StoreInterface Subject Weakly.Metadata -> GetEntryFromFile -> RepoWebhookEvent -> PushEvent' -> Handler ()
pushHook intf getEntryFromFile _ ev@(PushEvent' (Commit added modified removed) repoInfo) = liftIO $ do
  -- putStrLn $ (show . whUserLogin . evPullRequestSender) ev ++ " pullRequested a commit, resulting in the following event: "
  putStrLn $ BLC.unpack $ Aeson.encode ev

  traverse_ (writeEntry intf) (toList added)
  traverse_ (writeEntry intf) (toList modified)
  traverse_ (removeEntry intf) (toList removed)

  where
    toList :: Maybe (NonEmpty a) -> [a]
    toList = maybe mempty NE.toList

    writeEntry :: StoreInterface Subject Weakly.Metadata -> Text -> IO ()
    writeEntry (StoreInterface { storeWrite = write }) file = do
      mEntry <- getEntryFromFile repoInfo file
      case mEntry of
        Nothing    -> pure ()
        Just entry -> do
          let subject = Weakly.metaSubject entry
          write subject entry

    removeEntry :: StoreInterface Subject Weakly.Metadata -> Text -> IO ()
    removeEntry (StoreInterface { storeDelete = delete }) removedFile = do
      let removedFileStr = T.unpack removedFile
      case takeBaseName removedFileStr of
        ""      -> putStrLn $ "Not removing '" <> removedFileStr <> "' because it's not a file."
        subject -> delete (Subject $ T.pack subject)
