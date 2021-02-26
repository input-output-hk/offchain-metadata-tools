{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Metadata.Attestation where

import           Data.Aeson                   (ToJSON)
import qualified Data.Aeson                   as Aeson
import           Data.Foldable                (for_)
import           Data.ByteArray.Encoding      (Base (Base16), convertToBase)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict          as HM
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash
import qualified Cardano.Metadata.Types as Submitter
import Data.Validation (Validation(Failure, Success))

import qualified Cardano.Metadata.Types.Weakly as Weakly
import Cardano.Metadata.Types.Common

hashesForAttestation :: ToJSON val => Subject -> PropertyName -> val -> Submitter.HashesForAttestation
hashesForAttestation (Subject subj) (PropertyName pName) v =
  Submitter.hashesForAttestation
    (Submitter.Subject subj)
    (Submitter.Property pName)
    (mkPropertyValue v)

mkPropertyValue :: ToJSON val => val -> Submitter.PropertyValue
mkPropertyValue v = Submitter.PropertyValue (T.decodeUtf8 $ BSL.toStrict $ Aeson.encode v) (Aeson.toJSON v)

data AttestationError
  = AttestationFailed Subject PropertyName Aeson.Value AnnotatedSignature Text
  deriving (Eq, Show)

prettyPrintAttestationError :: AttestationError -> Text
prettyPrintAttestationError (AttestationFailed subj pName val (AnnotatedSignature sig publicKey) err) =
  let
    hashes       = hashesForAttestation subj pName val
    expectedHash = T.decodeUtf8 $ hashToBytes $ Submitter.attestationDigest hashes
    sigPretty    = T.decodeUtf8 $ convertToBase Base16 $ rawSerialiseSigDSIGN sig
    pubKeyPretty = T.decodeUtf8 $ convertToBase Base16 $ rawSerialiseVerKeyDSIGN publicKey
    subjHash     = T.decodeUtf8 $ hashToBytes $ Submitter._hashesForAttestation_subject hashes
    propNameHash = T.decodeUtf8 $ hashToBytes $ Submitter._hashesForAttestation_property hashes
    valueHash    = T.decodeUtf8 $ hashToBytes $ Submitter._hashesForAttestation_value hashes
  in
    "The signature '" <> sigPretty <> "',"
    <> "annotated by the public key '" <> pubKeyPretty <> "', "
    <> "does not match the hash '" <> expectedHash <> "', "
    <> "derived from hash( "
    <> "hash(" <> T.pack (show subj) <> ")[ " <> subjHash <> " ]"
    <> "+ hash(" <> T.pack (show pName) <> ")[ " <> propNameHash <> " ] "
    <> "+ hash(" <> T.pack (show val) <> ")[" <> valueHash <> "] )."
    <> " Specifically, the error was: '" <> err <> "'."

isAttestedBy
  :: Submitter.HashesForAttestation
  -> AnnotatedSignature
  -> Either Text ()
isAttestedBy hashes (AnnotatedSignature sig publicKey) =
  Submitter.isAttestedBy
    hashes
    (Submitter.AttestationSignature publicKey sig)

validatePropertyAttestationSignatures
  :: ToJSON val
  => Subject
  -> PropertyName
  -> Property val
  -> Validation (NonEmpty AttestationError) ()
validatePropertyAttestationSignatures subj pName prop =
  let
    anSigs = propertyAnSignatures prop
    val    = propertyValue prop
    hashes = hashesForAttestation subj pName val
  in
    for_ anSigs $ \anSig ->
      case hashes `isAttestedBy` anSig of
        Left err -> Failure $ AttestationFailed subj pName (Aeson.toJSON val) anSig err NE.:| []
        Right x  -> Success x

validateMetadataAttestationSignatures
  :: Weakly.Metadata
  -> Validation (NonEmpty AttestationError) ()
validateMetadataAttestationSignatures (Weakly.Metadata subj props) =
  let
    propList :: [(PropertyName, Property Aeson.Value)]
    propList = HM.toList props
  in
    for_ propList $ \(pName, prop) ->
      validatePropertyAttestationSignatures subj pName prop
    
