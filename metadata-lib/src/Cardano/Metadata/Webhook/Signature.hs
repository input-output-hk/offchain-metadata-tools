{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | GitHub signs every webhook delivery with both a legacy SHA-1 HMAC
-- (@X-Hub-Signature@) and, since 2017, a SHA-256 HMAC (@X-Hub-Signature-256@)
-- of the same secret and body. The pinned servant-github-webhook dependency
-- (see the 'Cardano.Metadata.Webhook.Types.GitHubKey' combinator) only ever
-- verifies the legacy SHA-1 header.
--
-- This module adds an independent SHA-256 check as a WAI middleware, so a
-- request must satisfy /both/ checks to reach the application. This closes
-- the gap without needing to fork or bump that dependency.
module Cardano.Metadata.Webhook.Signature
  ( requireHubSignature256
  , signBodySha256
  ) where

import Crypto.Hash.Algorithms ( SHA256 )
import Crypto.MAC.HMAC ( HMAC (hmacGetDigest), hmac )
import Data.ByteArray ( constEq )
import Data.ByteArray.Encoding ( Base (Base16), convertToBase )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.IORef ( newIORef, readIORef, writeIORef )
import Network.HTTP.Types ( status401 )
import Network.Wai
    ( Middleware
    , Request (requestHeaders)
    , responseLBS
    , setRequestBodyChunks
    , strictRequestBody
    )

-- | Reject any request that doesn't carry a valid @X-Hub-Signature-256@ for
-- the given shared secret before it reaches the wrapped application.
requireHubSignature256 :: BS.ByteString -> Middleware
requireHubSignature256 secret app req sendResponse = do
  body <- BSL.toStrict <$> strictRequestBody req
  case lookup "X-Hub-Signature-256" (requestHeaders req) of
    Nothing    -> reject "Missing X-Hub-Signature-256 header."
    Just sigHdr
      | constEq (BS.drop 7 sigHdr) (hexHmacSha256 secret body) -> do
          -- The body has already been fully consumed above (strictRequestBody
          -- reads to the end), so replay the buffered bytes for whatever
          -- reads it next (e.g. Servant's own body parsing/signature check).
          req' <- replayBody body
          app req' sendResponse
      | otherwise -> reject "X-Hub-Signature-256 does not match."
  where
    reject msg = sendResponse $ responseLBS status401 [] (BSL.fromStrict msg)

    replayBody body = do
      remaining <- newIORef (Just body)
      let chunks = do
            mChunk <- readIORef remaining
            case mChunk of
              Nothing    -> pure BS.empty
              Just chunk -> writeIORef remaining Nothing >> pure chunk
      pure (setRequestBodyChunks chunks req)

hexHmacSha256 :: BS.ByteString -> BS.ByteString -> BS.ByteString
hexHmacSha256 secret body =
  convertToBase Base16 (hmacGetDigest (hmac secret body :: HMAC SHA256))

-- | Compute the @X-Hub-Signature-256@ header value GitHub would send for a
-- given secret and request body, e.g. for use in tests.
signBodySha256 :: BS.ByteString -> BS.ByteString -> BS.ByteString
signBodySha256 secret body = "sha256=" <> hexHmacSha256 secret body
