{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Wallet-specific validation rules.
-}

module Cardano.Metadata.Validation.Wallet
  ( WalletValidationError(..)
  , prettyPrintWalletValidationError
  , walletValidationRules
  , walletValidation
  ) where

import Cardano.Metadata.GoguenRegistry
    ( parseRegistryEntry, validateEntry )
import Cardano.Metadata.Types.Common
    ( File, fileContents )
import Cardano.Metadata.Validation.Rules
    ( Transform, ValidationError (ErrorCustom), defaultRules, mkTransform )
import Cardano.Metadata.Validation.Types
    ( Difference (Added, Changed, Removed), invalid, valid )
import qualified Cardano.Metadata.Validation.Types as Common
    ( Metadata )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Text
    ( Text )
import Data.Validation
    ( Validation )
import Prelude hiding
    ( log )

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T

data WalletValidationError
  = WalletFailedToParseRegistryEntry String
  -- ^ Failed to parse a metadata entry from the file contents (json value, err)
  | WalletFailedToValidate Text
  -- ^ Failed to validate the metadata entry (json value, err)
  deriving (Eq, Show)

-- | Pretty print a wallet-specific validation error.
prettyPrintWalletValidationError :: WalletValidationError -> Text
prettyPrintWalletValidationError (WalletFailedToParseRegistryEntry err) =
  "Failed to parse wallet metadata entry from JSON, error was: " <> T.pack err <> "."
prettyPrintWalletValidationError (WalletFailedToValidate err) =
  "Failed to validate wallet metadata entry, error was: '" <> err <> "'."

-- | The wallet validation rules consist of the default validation
-- rules for metadata, as well as some wallet-specific validation (see
-- @walletValidation@).
walletValidationRules
  :: Transform
       (Difference (File Common.Metadata))
       (Validation (NonEmpty (ValidationError WalletValidationError)))
       ()
walletValidationRules =
  defaultRules
  *> mkTransform walletValidation

-- | Try to parse and validate a wallet metadata entry (see
-- @parseRegistryEntry@ and @validateEntry@).
walletValidation
  :: Difference (File Common.Metadata)
  -> Validation (NonEmpty (ValidationError WalletValidationError)) ()
walletValidation diff = do
  -- Find the new file we are going to validate
  let
    mMeta =
      case diff of
        Added newFile     -> Just (fileContents newFile)
        Removed _         -> Nothing
        Changed _ newFile -> Just (fileContents newFile)

  case mMeta of
    -- If we've removed a file, don't need to perform wallet validation
    Nothing      -> valid
    Just newMeta -> do
      let json = Aeson.toJSON newMeta
      case Aeson.parseEither parseRegistryEntry json of
        Left e      ->
          invalid (ErrorCustom $ WalletFailedToParseRegistryEntry e)
        Right entry -> do
          case validateEntry entry of
            Left e -> do
              invalid (ErrorCustom $ WalletFailedToValidate e)
            Right () ->
              valid

