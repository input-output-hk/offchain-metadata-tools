{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Metadata.Webhook.API where

import           Servant
import           Servant.GitHub.Webhook         (GitHubEvent,
                                                 GitHubSignedReqBody,
                                                 RepoWebhookEvent (..))

import           Cardano.Metadata.Webhook.Types

type MetadataWebhookAPISigned   = MetadataWebhookAPIF GitHubSignedReqBody
type MetadataWebhookAPIUnsigned = MetadataWebhookAPIF ReqBody

type MetadataWebhookAPIF reqBody = "webhook" :> PushHookAPIF reqBody

type PushHookAPIF reqBody
  =  GitHubEvent '[ 'WebhookPushEvent ]
  :> reqBody '[JSON] PushEvent'
  :> Post '[JSON] ()
