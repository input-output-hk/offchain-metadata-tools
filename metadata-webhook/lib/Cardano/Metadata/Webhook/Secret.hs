module Cardano.Metadata.Webhook.Secret
  ( resolveWebhookSecret
  ) where

import qualified Data.ByteString.Char8 as C8

-- | Turn the raw value of the METADATA_WEBHOOK_SECRET environment variable
-- into an HMAC key, rejecting both the unset and empty-string cases.
--
-- servant-github-webhook's 'gitHubKey' wraps whatever key it is given as
-- @Just key@ and always runs signature verification against it, even when
-- @key@ is empty. An empty key is a publicly known key, so falling back to
-- @mempty@ here would let anyone forge a valid X-Hub-Signature.
resolveWebhookSecret :: Maybe String -> Either String C8.ByteString
resolveWebhookSecret Nothing =
  Left "METADATA_WEBHOOK_SECRET environment variable is not set."
resolveWebhookSecret (Just s)
  | null s    = Left "METADATA_WEBHOOK_SECRET environment variable is empty."
  | otherwise = Right (C8.pack s)
