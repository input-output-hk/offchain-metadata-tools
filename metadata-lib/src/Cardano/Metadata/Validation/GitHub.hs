{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Metadata.Validation.GitHub
  (
  -- * Constructors
    GitHubFileStatus(..)
  , GitHubFile(GitHubFile)
  , GitHubPullRequest(GitHubPullRequest)
  , PullRequestValidationError(..)
  , ExpectedBaseBranch(ExpectedBaseBranch)

  -- * Views
  , prettyPrintPRValidationError
  , fileName
  , fileStatus
  , prBaseBranch
  , prNumChangedFiles

  -- * Conversions
  , fromGHFileStatus
  , fromGHFile
  , fromGHPullRequest

  -- * Rules
  , gitHubValidationRules
  , validatePR
  , validatePRFile
  ) where


import           Colog                         (pattern D, pattern E, pattern I, pattern W,
                                                Message, WithLog,
                                                log)
import Data.String (IsString)
import Data.Text (Text)
import Prelude hiding (log)
import qualified Data.Text as T
import Control.Monad.Except (MonadError, throwError)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Quiet (Quiet(Quiet))
import qualified GitHub
import GHC.Generics (Generic)

newtype ExpectedBaseBranch = ExpectedBaseBranch Text
  deriving (Generic, Eq, Ord)
  deriving newtype (IsString)
  deriving (Show) via (Quiet ExpectedBaseBranch)

data PullRequestValidationError
  = PRTargetsWrongBranch ExpectedBaseBranch Text
  -- ^ Pull request targets wrong branch (expected, actual)
  | PRDoesntModifyAnyFiles Int
  -- ^ Pull request didn't add or modify any files (num files modified)
  | PRRemovingRecordNotPermitted Text
  -- ^ Pull request tried to remove a record (filename)
  | PRRenamingRecordNotPermitted Text
  -- ^ Pull request tried to rename a record (filename)
  deriving (Eq, Show)


prettyPrintPRValidationError :: PullRequestValidationError -> Text
prettyPrintPRValidationError (PRTargetsWrongBranch (ExpectedBaseBranch expected) actual) =
  "Wanted base branch '" <> expected <> "' but got '" <> actual <> "'."
prettyPrintPRValidationError (PRDoesntModifyAnyFiles changedFiles) =
  T.pack $ "Pull request must add or modify at least one file, but it changed " <> show changedFiles <> " files."
prettyPrintPRValidationError (PRRemovingRecordNotPermitted filename) =
  "Pull request tried to remove file '" <> filename <> "', but removing entries is not permitted."
prettyPrintPRValidationError (PRRenamingRecordNotPermitted filename) =
  "Pull request tried to rename file '" <> filename <> "', but renaming entries is not permitted."

data GitHubFileStatus = Added
                      | Modified
                      | Renamed
                      | Removed
                      | Unknown Text
  deriving (Eq, Show)

data GitHubFile
  = GitHubFile { ghFileName   :: Text
               , ghFileStatus :: GitHubFileStatus
               }
  deriving (Eq, Show)

fileName :: GitHubFile -> Text
fileName = ghFileName

fileStatus :: GitHubFile -> GitHubFileStatus
fileStatus = ghFileStatus

data GitHubPullRequest
  = GitHubPullRequest { ghPRBaseBranch      :: Text
                      , ghPRNumChangedFiles :: Int
                      } 
  deriving (Eq, Show)

prBaseBranch :: GitHubPullRequest -> Text
prBaseBranch = ghPRBaseBranch

prNumChangedFiles :: GitHubPullRequest -> Int
prNumChangedFiles = ghPRNumChangedFiles

fromGHFileStatus :: Text -> GitHubFileStatus
fromGHFileStatus "added"    = Added
fromGHFileStatus "modified" = Modified
fromGHFileStatus "renamed"  = Renamed
fromGHFileStatus "removed"  = Removed
fromGHFileStatus x          = Unknown x

fromGHFile :: GitHub.File -> GitHubFile
fromGHFile f = GitHubFile (GitHub.fileFilename f) (fromGHFileStatus $ GitHub.fileStatus f)

fromGHPullRequest :: GitHub.PullRequest -> GitHubPullRequest
fromGHPullRequest pr =
  GitHubPullRequest
    (pr & GitHub.pullRequestBase & GitHub.pullRequestCommitRef)
    (pr & GitHub.pullRequestChangedFiles)

gitHubValidationRules
  :: ( WithLog env Message m
     , MonadError PullRequestValidationError m
     )
  => ExpectedBaseBranch
  -> GitHubPullRequest
  -> [GitHubFile]
  -> m ()
gitHubValidationRules expectedBaseBranch pr files = do
  log D $ T.pack $ "Validating pull request: '" <> show pr <> "'."

  _ <- validatePR expectedBaseBranch pr 

  log D $ T.pack $ "Validating pull request files: '" <> show files <> "'."
  log I $ T.pack $ "Validating " <> show (pr & prNumChangedFiles) <> " files."

  traverse_ validatePRFile files

validatePR
  :: ( WithLog env Message m
     , MonadError PullRequestValidationError m
     )
  => ExpectedBaseBranch
  -> GitHubPullRequest
  -> m ()
validatePR (ExpectedBaseBranch expectedBaseBranch) pr = do
    let
      baseBranch   = prBaseBranch pr
      changedFiles = prNumChangedFiles pr

    if baseBranch /= expectedBaseBranch
      then do
        let err = PRTargetsWrongBranch (ExpectedBaseBranch expectedBaseBranch) baseBranch
        log E $ prettyPrintPRValidationError err
        throwError err
      else pure ()

    if changedFiles <= 0
      then do
        let err = PRDoesntModifyAnyFiles changedFiles
        log E $ prettyPrintPRValidationError err
        throwError err
      else pure ()

validatePRFile
  :: ( WithLog env Message m
     , MonadError PullRequestValidationError m
     )
  => GitHubFile
  -> m ()
validatePRFile file =
  let
    filename = fileName file
  in
    case fileStatus file of
      Removed     -> do
        let
          err = PRRemovingRecordNotPermitted filename
        log E $ prettyPrintPRValidationError err
        throwError err
      Renamed     -> do
        let
          err = PRRenamingRecordNotPermitted filename
        log E $ prettyPrintPRValidationError err
        throwError err
      Modified    -> log I "Modifying a record..."
      Added       -> log I "Adding a record..."
      (Unknown x) -> log W ("Unknown status '" <> x <> "' is not recognized, ignoring.")
