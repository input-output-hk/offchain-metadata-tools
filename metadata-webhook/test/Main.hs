{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import qualified Data.ByteString.Char8 as C8
import Data.Either ( isLeft )
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( assertBool, testCase, (@?=) )

import Cardano.Metadata.Webhook.Secret ( resolveWebhookSecret )

main :: IO ()
main = defaultMain tests

-- | These are the tests that guard against the process ever falling back to
-- an empty HMAC key: an empty key is a publicly known key, so if
-- resolveWebhookSecret ever let one through, anyone could forge a valid
-- X-Hub-Signature against the webhook.
tests :: TestTree
tests = testGroup "Cardano.Metadata.Webhook.Secret"
  [ testCase "an unset METADATA_WEBHOOK_SECRET is refused" $
      assertRefused (resolveWebhookSecret Nothing)

  , testCase "an empty METADATA_WEBHOOK_SECRET is refused" $
      assertRefused (resolveWebhookSecret (Just ""))

  , testCase "a non-empty METADATA_WEBHOOK_SECRET is accepted" $
      resolveWebhookSecret (Just "sekrit") @?= Right (C8.pack "sekrit")
  ]
  where
    assertRefused = assertBool "expected the secret to be refused (Left)" . isLeft
