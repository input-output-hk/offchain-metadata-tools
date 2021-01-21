module Main where

import           Control.Monad.IO.Class       ( liftIO )
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as C8
import           Data.Maybe                   ( fromJust )
import           GitHub.Data.Webhooks.Events  ( PushEvent(..), IssueCommentEvent(..) )
import           GitHub.Data.Webhooks.Payload ( HookIssueComment(..), HookUser(..) )
import           Network.Wai                  ( Application )
import           Network.Wai.Handler.Warp     ( run )
import           System.Environment           ( lookupEnv )
import qualified Data.Aeson as Aeson

-- Using servant and servant-github-webhook to serve the API
import           Servant
import qualified Servant.GitHub.Webhook       as SGH
import           Servant.GitHub.Webhook       ( GitHubEvent, GitHubSignedReqBody, RepoWebhookEvent(..) )

-- Lib
newtype GitHubKey = GitHubKey (forall result. SGH.GitHubKey result)

gitHubKey :: IO BS.ByteString -> GitHubKey
gitHubKey k = GitHubKey (SGH.gitHubKey k)

instance HasContextEntry '[GitHubKey] (SGH.GitHubKey result) where
    getContextEntry (GitHubKey x :. _) = x

-- API defn.
type MetadataWebhookAPI = "webhook" :> PushHookAPI

type PushHookAPI
  =  GitHubEvent '[ 'WebhookPushEvent ]
  :> GitHubSignedReqBody '[JSON] Aeson.Object
  :> Post '[JSON] ()

-- API handlers
pushHook :: RepoWebhookEvent -> ((), Aeson.Object) -> Handler ()
pushHook _ (_, ev) = liftIO $ do
    putStrLn $ (show . whUserLogin . evPushSender) ev ++ " pushed a commit, resulting in the following event: "
    putStrLn $ show ev

metadataWebhook :: Server MetadataWebhookAPI
metadataWebhook = pushHook

main :: IO ()
main = do
    port <- maybe 8080 read <$> lookupEnv "PORT"
    key <- maybe mempty C8.pack <$> lookupEnv "KEY"
    putStrLn $ "Server is starting on port " ++ show port ++ " using test secret " ++ show key
    putStrLn $ "Perhaps run 'ngrok http " ++ show port ++ "' for a forwarding address"
    run port (app (gitHubKey $ pure key))

app :: GitHubKey -> Application
app key
  = serveWithContext
    (Proxy :: Proxy MetadataWebhookAPI)
    (key :. EmptyContext)
    metadataWebhook

-- TODO Take "write" and "read" functions as arguments to a function, expose that function
-- Allows users of this library to easily choose their DB backend
-- We'll use psql database backend here.

-- TODO Ensure file size < 400kb
