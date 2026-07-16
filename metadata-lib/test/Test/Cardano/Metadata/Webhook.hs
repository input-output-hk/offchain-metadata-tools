{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Metadata.Webhook
  ( tests
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Types ( status200, status401 )
import Network.Wai
    ( Application
    , Request (requestHeaders)
    , defaultRequest
    , responseLBS
    , strictRequestBody
    )
import Network.Wai.Test
    ( SRequest (SRequest), runSession, simpleBody, simpleStatus, srequest )
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Cardano.Metadata.Webhook.Signature
    ( requireHubSignature256, signBodySha256 )
import Cardano.Metadata.Webhook.Types ( GitHubToken (..), resolveGithubToken )

tests :: TestTree
tests = testGroup "Cardano.Metadata.Webhook"
  [ tokenTests
  , signatureTests
  ]

-- | An absent GitHub token is a legitimate configuration (for public
-- repositories), so resolveGithubToken must never fail closed on it --
-- unlike the webhook secret, both an unset and an empty-string value are
-- normalized to "no token" rather than being treated as an error.
tokenTests :: TestTree
tokenTests = testGroup "Cardano.Metadata.Webhook.Types"
  [ testCase "an unset METADATA_GITHUB_TOKEN resolves to no token" $
      resolveGithubToken Nothing @?= Nothing

  , testCase "an empty METADATA_GITHUB_TOKEN resolves to no token" $
      resolveGithubToken (Just "") @?= Nothing

  , testCase "a non-empty METADATA_GITHUB_TOKEN resolves to that token" $
      resolveGithubToken (Just "ghp_realtoken") @?= Just (GitHubToken "ghp_realtoken")
  ]

-- | requireHubSignature256 is the only thing in this repo that checks
-- GitHub's SHA-256 signature (the pinned servant-github-webhook dependency
-- only checks the legacy SHA-1 one); these confirm it actually rejects an
-- unsigned/mis-signed request, and that a correctly signed request still
-- reaches the wrapped application with its body intact.
signatureTests :: TestTree
signatureTests = testGroup "Cardano.Metadata.Webhook.Signature"
  [ testCase "a correctly signed request reaches the app with its body intact" $ do
      resp <- runSession (srequest (signedWith secret)) (requireHubSignature256 secret echoApp)
      simpleStatus resp @?= status200
      simpleBody resp @?= body

  , testCase "a missing X-Hub-Signature-256 header is rejected" $ do
      resp <- runSession (srequest (SRequest defaultRequest body)) (requireHubSignature256 secret echoApp)
      simpleStatus resp @?= status401

  , testCase "an incorrect X-Hub-Signature-256 is rejected" $ do
      resp <- runSession (srequest (signedWith "wrong-secret")) (requireHubSignature256 secret echoApp)
      simpleStatus resp @?= status401
  ]
  where
    secret = "super-secret"
    body   = "{\"hello\":\"world\"}"

    echoApp :: Application
    echoApp req respond = strictRequestBody req >>= respond . responseLBS status200 []

    signedWith :: BS.ByteString -> SRequest
    signedWith withSecret =
      SRequest
        defaultRequest { requestHeaders = [("X-Hub-Signature-256", signBodySha256 withSecret (BSL.toStrict body))] }
        body
