{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Metadata.Validation.Rules
  (
  -- * Error types
    ValidationError(..)
  , ValidationError_
  , prettyPrintValidationError
  , prettyPrintValidationError_

  -- * Rules
  , defaultRules
  , maxFileSizeBytes
  , metadataJSONMaxSize
  , subjectMatchesFileName
  , sequenceNumber
  , sequenceNumbers
  , isJSONFile
  , baseFileNameLengthBounds

  -- * Helpers
  , toAttestedPropertyDiffs

  -- * Re-exports
  , Transform.apply
  , Transform.apply_
  , Transform.mkTransform
  , Transform.mkTransform_
  , Transform.Transform
  , Transform.Transform_
  ) where

import qualified Data.Aeson                        as Aeson
import           Data.Bool                         (bool)
import           Data.Foldable                     (traverse_)
import           Data.Function                     ((&))
import           Data.List.NonEmpty                (NonEmpty)
import qualified Data.Map.Merge.Strict             as M
import qualified Data.Map.Strict                   as M
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Validation                   (Validation)
import           Data.Void                         (Void, absurd)
import           Numeric.Natural                   (Natural)

import           Cardano.Metadata.Transform
import qualified Cardano.Metadata.Transform        as Transform
import           Cardano.Metadata.Types.Common
import           Cardano.Metadata.Validation.Types

data ValidationError e = ErrorMetadataFileTooBig Natural Natural
                       -- ^ Size of metadata file in bytes exceeds
                       -- maximum (maximum, actual)
                       | ErrorMetadataFileNameDoesntMatchSubject Subject Text
                       -- ^ Name of the file does not match the
                       -- subject (subject, fileName). This is a
                       -- requirement because otherwise an attacker
                       -- could submit conflicting entries for an
                       -- existing subject by using a different
                       -- file name.
                       | ErrorMetadataPropertySequenceNumberMustBeLarger (AttestedProperty Aeson.Value) (AttestedProperty Aeson.Value) SequenceNumber SequenceNumber
                       -- ^ The value of the property changed but the
                       -- sequence number did not increase (propery
                       -- old, property new, old sequence num, new
                       -- sequence num)
                       | ErrorMetadataFileExpectedExtension FilePath Text Text
                       -- ^ On the given file, the file extension did
                       -- not match the expected extension (filePath,
                       -- expected extension, actual extension).
                       | ErrorMetadataFileBaseNameLengthBounds Text (Natural, Natural) Natural
                       -- ^ For the given file base name, it's length
                       -- was expected to be within the inclusive
                       -- range but wasn't (file base name, (minimum,
                       -- maximum), actual).
                       | ErrorCustom e
  deriving (Eq, Show)

type ValidationError_ = ValidationError Void

prettyPrintValidationError :: (e -> Text) -> ValidationError e -> Text
prettyPrintValidationError _(ErrorMetadataFileTooBig limit actual) =
  "File size in bytes (" <> T.pack (show actual) <> " bytes) greater than maximum size of " <> T.pack (show limit) <> " bytes."
prettyPrintValidationError _ (ErrorMetadataFileNameDoesntMatchSubject subject fileName) =
  "Metadata subject '" <> unSubject subject <> "', does not match file name '" <> fileName <> "'."
prettyPrintValidationError _ (ErrorMetadataPropertySequenceNumberMustBeLarger oldProp newProp oldSeq newSeq) =
  "Property '" <> T.pack (show oldProp) <> "' has changed to '" <> T.pack (show newProp) <> "', but the new sequence number '" <> T.pack (show $ seqToInteger newSeq) <> "' is not greater than the old sequence number '" <> T.pack (show $ seqToInteger oldSeq) <> "'."
prettyPrintValidationError _ (ErrorMetadataFileExpectedExtension path expected actual) =
  "For the file '" <> T.pack path <> "', expected the extension '" <> expected <> "', but got '" <> actual <> "'."
prettyPrintValidationError _ (ErrorMetadataFileBaseNameLengthBounds baseName (mini, maxi) actual) =
  "The file base name '" <> baseName <> "', must be within the range [" <> T.pack (show mini) <> ", " <> T.pack (show maxi) <> "] (inclusive), but was actually '" <> T.pack (show actual) <> "'."
prettyPrintValidationError customFn (ErrorCustom e) =
  customFn e

prettyPrintValidationError_ :: ValidationError_ -> Text
prettyPrintValidationError_ = prettyPrintValidationError absurd

-- | Default metadata validation rules.
defaultRules
  :: Transform
       (Difference (File Metadata))
       (Validation (NonEmpty (ValidationError e)))
       ()
defaultRules =
     mkTransform (maxFileSizeBytes metadataJSONMaxSize)
  *> mkTransform subjectMatchesFileName
  *> mkTransform sequenceNumbers
  *> mkTransform isJSONFile
  *> mkTransform (baseFileNameLengthBounds 1 256)

-- | Ensure that the file has the ".json" extension.
isJSONFile
  :: Difference (File a)
  -> Validation (NonEmpty (ValidationError e)) ()
isJSONFile diff =
  let
    mFile = case diff of
      Added newFile     -> Just newFile
      Changed _ newFile -> Just newFile
      -- If a file is removed, we don't need to check it's extension
      Removed _         -> Nothing
  in
    case mFile of
      Nothing   -> valid
      Just file ->
        let
          exts = fileExtensions file
          path = filePath file
        in
          if exts /= ".json"
          then invalid $ ErrorMetadataFileExpectedExtension path ".json" exts
          else valid

-- | Ensure that the length of the base name of a file is within the
-- (inclusive) range provided.
--
-- The base name of "/directory/file.ext" is "file", which is within
-- the length range [1,4].
baseFileNameLengthBounds
  :: Natural
  -- ^ Minimum (inclusive)
  -> Natural
  -- ^ Maximum (inclusive)
  -> Difference (File a)
  -> Validation (NonEmpty (ValidationError e)) ()
baseFileNameLengthBounds mini maxi diff =
  let
    mFile = case diff of
      Added newFile     -> Just newFile
      Changed _ newFile -> Just newFile
      -- If a file is removed, we don't need to check it's name
      Removed _         -> Nothing
  in
    case mFile of
      Nothing   -> valid
      Just file ->
        let
          baseName = fileBaseName file
          len      = fromIntegral $ T.length baseName
        in
          if len >= mini && len <= maxi
          then valid
          else invalid $ ErrorMetadataFileBaseNameLengthBounds baseName (mini, maxi) len


-- | Ensure that the size (in bytes) of a new metadata entry file does
-- not exceed the given amount.
maxFileSizeBytes
  :: Natural
  -> Difference (File a)
  -> Validation (NonEmpty (ValidationError e)) ()
maxFileSizeBytes limit diff =
  let
    mFileSize = case diff of
      Added newFile     -> Just $ fileSize newFile
      Changed _ newFile -> Just $ fileSize newFile
      Removed _         -> Nothing
  in
    case mFileSize of
      Nothing   -> valid
      Just size -> if size > limit
                   then invalid (ErrorMetadataFileTooBig limit size)
                   else valid

-- | Ensure that the subject of a metadata file matches the name of
-- that file. This is so an attacker cannot overwrite entries for an
-- existing subject by submitting an entry of the same subject under a
-- new file name.
subjectMatchesFileName
  :: Difference (File Metadata)
  -> Validation (NonEmpty (ValidationError e)) ()
subjectMatchesFileName diff =
  let
    mFile = case diff of
      Added     newFile -> Just newFile
      Changed _ newFile -> Just newFile
      Removed _         -> Nothing
  in
    case mFile of
      Nothing    -> valid
      Just file  ->
        let
          subject = metaSubject $ fileContents file
          fileNameNoExt = fileBaseName file
        in
          if fileNameNoExt /= unSubject subject
          then invalid (ErrorMetadataFileNameDoesntMatchSubject subject fileNameNoExt)
          else valid

-- | Ensure that if a property has changed, it's sequence number has
-- increased. The sequence number of properties added or removed are
-- ignored.
sequenceNumber
  :: Difference (AttestedProperty Aeson.Value)
  -> Validation (NonEmpty (ValidationError e)) ()
-- The sequence numbers of properties added or removed are not checked
sequenceNumber (Added _)                 = valid
sequenceNumber (Removed _)               = valid
-- If property is different, sequence number must be larger.
sequenceNumber (Changed oldProp newProp) =
  let
    oldSeqNum = attestedSequenceNumber oldProp
    newSeqNum = attestedSequenceNumber newProp
  in
    newSeqNum > oldSeqNum
    & bool
      (invalid $ ErrorMetadataPropertySequenceNumberMustBeLarger oldProp newProp oldSeqNum newSeqNum)
      valid

-- | Runs @sequenceNumber@ on each attested property diff in a file
-- diff, i.e. "lifts" the @sequenceNumber@ function to work on @File
-- Metadata@.
sequenceNumbers
  :: Difference (File Metadata)
  -> Validation (NonEmpty (ValidationError e)) ()
sequenceNumbers = traverse_ sequenceNumber . toAttestedPropertyDiffs

-- | Interpret a @File Metadata@ diff as a series of diffs on each
-- @AttestedProperty@. If the @File@ is removed, all properties are
-- considered removed, and if a file is added, all properties are
-- considered added. If a file is changed, we match properties on
-- their names and generate appropriate an @Difference@ based on
-- whether they have changed, been added, or been removed.
toAttestedPropertyDiffs
  :: Difference (File Metadata)
  -> [Difference (AttestedProperty Aeson.Value)]
toAttestedPropertyDiffs (Added added) =
  let
    addedProps = M.elems $ metaAttestedProperties $ fileContents added
  in
    Added <$> addedProps
toAttestedPropertyDiffs (Removed removed) =
  let
    removedProps = M.elems $ metaAttestedProperties $ fileContents removed
  in
    Removed <$> removedProps
toAttestedPropertyDiffs (Changed old new) =
  let
    attestedOld = metaAttestedProperties $ fileContents old
    attestedNew = metaAttestedProperties $ fileContents new

    -- Properties present in both that have been modified are considered changed
    changed = M.zipWithMaybeMatched (\_key a b -> if a == b then Nothing else Just (Changed a b))
    -- Properties present only in new have been added
    added   = M.mapMissing (\_key add -> Added add)
    -- Properties present only in old have been removed
    removed = M.mapMissing (\_key rmd -> Removed rmd)
  in
    M.elems $ M.merge removed added changed attestedOld attestedNew

-- | Maximum size in bytes of a metadata entry.
metadataJSONMaxSize :: Natural
metadataJSONMaxSize = 380000
