{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Metadata.Webhook.API where

import           Control.Lens                   ((.~), (^.))
import           Control.Monad.IO.Class         (liftIO)
import           Data.Aeson                     (FromJSON, ToJSON, parseJSON,
                                                 toJSON, (.:), (.:?))
import qualified Data.Aeson                     as Aeson
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as C8
import qualified Data.ByteString.Lazy.Char8     as BLC
import           Data.Function                  ((&))
import qualified Data.HashMap.Strict            as HM
import           Data.List.NonEmpty             (NonEmpty ((:|)))
import qualified Data.List.NonEmpty             as NE
import           Data.Maybe                     (fromMaybe)
import           Data.Maybe                     (fromJust)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           GitHub.Data.Webhooks.Events    (IssueCommentEvent (..),
                                                 PullRequestEvent (..),
                                                 PushEvent (..))
import           GitHub.Data.Webhooks.Payload   (HookIssueComment (..),
                                                 HookUser (..))
import           Network.HTTP.Client            (requestBody)
import           Network.HTTP.Types             (hAccept, hAuthorization,
                                                 hUserAgent)
import           Network.HTTP.Types.Status      (Status, ok200)
import           Network.Wai                    (Application, Request,
                                                 getRequestBodyChunk)
import           Network.Wai.Handler.Warp       (defaultSettings, runSettings,
                                                 setLogger, setPort)
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wreq                   as Wreq
import           Servant
import           Servant.GitHub.Webhook         (GitHubEvent,
                                                 GitHubSignedReqBody,
                                                 RepoWebhookEvent (..))
import qualified Servant.GitHub.Webhook         as SGH
import           System.Environment             (lookupEnv)

import           Cardano.Metadata.Store.Types   (StoreInterface (..))
import           Cardano.Metadata.Types.Common  (AnnotatedSignature)
import           Cardano.Metadata.Webhook.Types

type MetadataWebhookAPISigned   = MetadataWebhookAPIF GitHubSignedReqBody
type MetadataWebhookAPIUnsigned = MetadataWebhookAPIF ReqBody

type MetadataWebhookAPIF reqBody = "webhook" :> PushHookAPIF reqBody

type PushHookAPIF reqBody
  =  GitHubEvent '[ 'WebhookPushEvent ]
  :> reqBody '[JSON] PushEvent'
  :> Post '[JSON] ()
