{-# LANGUAGE OverloadedStrings #-}

module Cardano.Metadata.Webhook.Server where

import Control.Lens ( (.~), (^.) )
import Control.Monad.IO.Class ( liftIO )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Foldable ( traverse_ )
import Data.Function ( (&) )
import Data.List.NonEmpty ( NonEmpty )
import qualified Data.List.NonEmpty as NE
import Data.Text ( Text )
import qualified Data.Text as T
import Network.HTTP.Types ( hAccept, hAuthorization, hUserAgent )
import Network.HTTP.Types.Status ( ok200 )
import qualified Network.Wreq as Wreq
import Servant
import Servant.GitHub.Webhook ( RepoWebhookEvent (..) )
import System.FilePath.Posix ( takeBaseName )

import Cardano.Metadata.Store.Types ( StoreInterface (..) )
import Cardano.Metadata.Types.Common ( Subject (Subject) )
import qualified Cardano.Metadata.Types.Weakly as Weakly
import Cardano.Metadata.Webhook.API
import Cardano.Metadata.Webhook.Signature ( requireHubSignature256 )
import Cardano.Metadata.Webhook.Types


-- | The pinned servant-github-webhook dependency only verifies the legacy
-- SHA-1 'X-Hub-Signature' (via 'GitHubKey'). 'requireHubSignature256' wraps
-- the resulting 'Application' with an independent check of the SHA-256
-- 'X-Hub-Signature-256' header GitHub also sends, so a request must satisfy
-- both to reach the handlers.
appSigned :: BS.ByteString -> GitHubKey -> StoreInterface Subject Weakly.Metadata -> GetEntryFromFile -> Application
appSigned secret key intf getEntryFromFile
  = requireHubSignature256 secret
  $ serveWithContext
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

-- | Fetch a file's contents from the GitHub repository pinned by
-- 'GitHubRepo' -- never from a URL supplied by the webhook payload itself.
--
-- The webhook payload's repository info is only used to check that the
-- event actually belongs to the repository we're configured for; a push
-- event for any other repository is ignored before any request is made, so
-- our GitHub token is never attached to a request whose destination host an
-- attacker could influence (SSRF + credential exfiltration).
getFileContent :: Maybe GitHubToken -> GitHubRepo -> GetEntryFromFile
getFileContent mGithubToken expectedRepo repoInfo fileName
  | repoInfoFullName repoInfo /= ghRepoFullName expectedRepo = do
      putStrLn $ "Ignoring push event for repository '" <> T.unpack (repoInfoFullName repoInfo) <> "'; this webhook is only configured to serve '" <> T.unpack (ghRepoFullName expectedRepo) <> "'."
      pure Nothing
  | otherwise = do
      let fileContentsUrl = ghRepoContentsUrl expectedRepo <> fileName

      let baseOptions = Wreq.defaults & Wreq.header hAccept .~ ["application/vnd.github.v3.raw"]
                                      & Wreq.header hUserAgent .~ ["metadata-webhook"]
                                      & Wreq.checkResponse .~ (pure mempty)

          -- No token means an anonymous request, not a request with a bad
          -- (empty) credential -- so the Authorization header is omitted
          -- entirely rather than sent as "".
          options = case mGithubToken of
            Nothing                  -> baseOptions
            Just (GitHubToken token) -> baseOptions & Wreq.header hAuthorization .~ [C8.pack (T.unpack token)]

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
