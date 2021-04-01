{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Metadata.Validation.GitHub
  ( tests
  ) where

import Colog
    ( LoggerT (LoggerT), Message, runLoggerT, usingLoggerT )
import Control.Monad.Except
    ( Except, MonadError, catchError, lift, runExcept, throwError )
import Data.Foldable
    ( traverse_ )
import Hedgehog
    ( forAll, property, (===) )
import qualified Hedgehog as H
    ( Property )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
    ( TestTree, testGroup )
import Test.Tasty.Hedgehog

import qualified Test.Cardano.Metadata.Generators as Gen

import Cardano.Metadata.Validation.GitHub

tests :: TestTree
tests = testGroup "GitHub validation tests"
  [ testProperty "GitHub/validation/prFile" prop_validation_prFile
  , testProperty "GitHub/validation/pr" prop_validation_pr
  , testProperty "GitHub/validation/githubValidationRules" prop_validation_gitHubValidationRules
  ]

prop_validation_prFile :: H.Property
prop_validation_prFile = property $ do
  file <- forAll $ Gen.gitHubFile

  -- Adding and modifying a record is allowed
  runNoLogging (validatePRFile (GitHubFile (fileName file) Added))
    === (Right () :: Either PullRequestValidationError ())

  runNoLogging (validatePRFile (GitHubFile (fileName file) Modified))
    === (Right () :: Either PullRequestValidationError ())

  -- Unknown statuses are allowed but ignored
  x <- forAll $ Gen.text (Range.linear 0 64) Gen.unicode
  runNoLogging (validatePRFile (GitHubFile (fileName file) (Unknown x)))
    === (Right () :: Either PullRequestValidationError ())

  -- Removing and renaming files is not allowed
  runNoLogging (validatePRFile (GitHubFile (fileName file) Removed))
    === (Left $ PRRemovingRecordNotPermitted (fileName file))

  runNoLogging (validatePRFile (GitHubFile (fileName file) Renamed))
    === (Left $ PRRenamingRecordNotPermitted (fileName file))

prop_validation_pr :: H.Property
prop_validation_pr = property $ do
  pr <- forAll Gen.gitHubPullRequest

  -- PR must target expected base branch
  let
    expectedBaseBranch = ExpectedBaseBranch $ prBaseBranch pr
    wrongBaseBranch    = prBaseBranch pr <> "_bad"
    wrongBaseBranchPr  = GitHubPullRequest wrongBaseBranch (prNumChangedFiles pr)

  runNoLogging (validatePR expectedBaseBranch wrongBaseBranchPr)
    === (Left $ PRTargetsWrongBranch expectedBaseBranch wrongBaseBranch)

  runNoLogging (validatePR expectedBaseBranch pr)
    === (Right () :: Either PullRequestValidationError ())

  -- PR must change at least one file
  let
    prChangesNoFiles = GitHubPullRequest (prBaseBranch pr) 0

  runNoLogging (validatePR expectedBaseBranch prChangesNoFiles)
    === (Left $ PRDoesntModifyAnyFiles 0)

prop_validation_gitHubValidationRules :: H.Property
prop_validation_gitHubValidationRules = property $ do
  -- Validating using the composite rule should just be the same as
  -- validating using a composition of the constituent rules.
  expectedBaseBranch <- forAll $ ExpectedBaseBranch <$> Gen.text (Range.linear 0 64) Gen.unicode
  pr                 <- forAll Gen.gitHubPullRequest
  files              <- forAll $ Gen.list (Range.linear 0 5) Gen.gitHubFile

  runNoLogging (gitHubValidationRules expectedBaseBranch pr files)
    === runNoLogging (validatePR expectedBaseBranch pr *> traverse_ validatePRFile files)

instance MonadError e (LoggerT msg (Except e)) where
  throwError = lift . throwError
  catchError m h = LoggerT $ catchError (runLoggerT m) (runLoggerT . h)

runNoLogging :: LoggerT Message (Except e) a -> Either e a
runNoLogging = runExcept . usingLoggerT mempty
