{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Metadata.Webhook
  ( tests
  ) where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Cardano.Metadata.Webhook.Types ( GitHubToken (..), resolveGithubToken )

-- | An absent GitHub token is a legitimate configuration (for public
-- repositories), so resolveGithubToken must never fail closed on it --
-- unlike the webhook secret, both an unset and an empty-string value are
-- normalized to "no token" rather than being treated as an error.
tests :: TestTree
tests = testGroup "Cardano.Metadata.Webhook.Types"
  [ testCase "an unset METADATA_GITHUB_TOKEN resolves to no token" $
      resolveGithubToken Nothing @?= Nothing

  , testCase "an empty METADATA_GITHUB_TOKEN resolves to no token" $
      resolveGithubToken (Just "") @?= Nothing

  , testCase "a non-empty METADATA_GITHUB_TOKEN resolves to that token" $
      resolveGithubToken (Just "ghp_realtoken") @?= Just (GitHubToken "ghp_realtoken")
  ]
