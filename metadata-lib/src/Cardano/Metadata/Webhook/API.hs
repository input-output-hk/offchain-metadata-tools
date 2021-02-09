{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Cardano.Metadata.Webhook.API where

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

import           Cardano.Metadata.Webhook.Types
import           Cardano.Metadata.Server.Types (Subject, Entry', _eSubject)
import           Cardano.Metadata.Store.Types (StoreInterface(..))

type MetadataWebhookAPISigned   = MetadataWebhookAPIF GitHubSignedReqBody
type MetadataWebhookAPIUnsigned = MetadataWebhookAPIF ReqBody

type MetadataWebhookAPIF reqBody = "webhook" :> PushHookAPIF reqBody

type PushHookAPIF reqBody
  =  GitHubEvent '[ 'WebhookPushEvent ]
  :> reqBody '[JSON] PushEvent'
  :> Post '[JSON] ()
