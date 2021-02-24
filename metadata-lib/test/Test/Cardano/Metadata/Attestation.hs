{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Metadata.Attestation
  ( tests
  ) where

import qualified Data.Aeson                       as Aeson
import qualified Data.Bifunctor                   as Bifunctor
import Data.Foldable (traverse_, forM_)
import           Data.Function ((&))
import           Text.RawString.QQ
import qualified Data.HashMap.Strict              as HM
import           Data.Text                        (Text)
import           Hedgehog                         (forAll, property, (===))
import           Hedgehog.Internal.Property       (forAllT)
import qualified Hedgehog                         as H (Property)
import           Test.Tasty                       (TestTree, testGroup)
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit                 (Assertion, testCase, (@?=))

import qualified Test.Cardano.Metadata.Generators as Gen
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Seed as Crypto
import qualified Cardano.Crypto.DSIGN.Ed25519 as Crypto
import qualified Data.Validation as Validation

import           Cardano.Metadata.Types.Common    (PropertyName(PropertyName), Subject(Subject),
                                                   propertyAnSignatures,
                                                   propertyValue,
                                                   mkAnnotatedSignature)
import qualified Cardano.Metadata.Types.Weakly    as Weakly
import Cardano.Metadata.Attestation (validateMetadataAttestationSignatures, hashesForAttestation, isAttestedBy, prettyPrintAttestationError, validatePropertyAttestationSignatures)

tests :: TestTree
tests = testGroup "Metadata attestation tests"
  [ testProperty "Attestation/valid" prop_attestation_valid
  , testCase "Attestation/invalid/subject" unit_attestation_invalid_subject_fails
  , testCase "Attestation/invalid/propertyName" unit_attestation_invalid_propertyName_fails
  , testCase "Attestation/invalid/propertyValue" unit_attestation_invalid_propertyValue_fails
  , testProperty "Attestation/relationship-between/isAttestedBy/validatePropertyAttestationSignatures" prop_attestation_relationship_property
  , testProperty "Attestation/relationship-between/isAttestedBy/validateMetadataAttestationSignatures" prop_attestation_relationship_metadata
  , testCase "Attestation/golden" unit_attestation_golden
  ]

prop_attestation_valid :: H.Property
prop_attestation_valid = property $ do
  subj  <- forAll Gen.subject
  pName <- forAll Gen.propertyName
  pVal  <- forAll Gen.propertyValue
  skey  <- forAllT Gen.signingKey

  let
    anSig  = mkAnnotatedSignature skey subj pName pVal
    hashes = hashesForAttestation subj pName pVal

  hashes `isAttestedBy` anSig === Right ()

prop_attestation_relationship_property :: H.Property
prop_attestation_relationship_property = property $ do
  subj  <- forAll Gen.subject
  pName <- forAll Gen.propertyName
  prop  <- forAllT $ Gen.weaklyTypedProperty subj pName

  let
    pVal   = propertyValue prop
    anSigs = propertyAnSignatures prop
    hashes = hashesForAttestation subj pName pVal

    validator1 :: Either Text ()
    validator1 = traverse_ (hashes `isAttestedBy`) anSigs

    validator2 :: Either Text ()
    validator2 =
      validatePropertyAttestationSignatures subj pName prop
        & Validation.toEither
        & Bifunctor.first (foldMap prettyPrintAttestationError)

  validator1 === validator2

prop_attestation_relationship_metadata :: H.Property
prop_attestation_relationship_metadata = property $ do
  subj <- forAll Gen.subject
  meta@(Weakly.Metadata _subj props) <- forAllT $ Gen.weaklyTypedMetadata subj

  let
    validator1 :: Either Text ()
    validator1 =
      forM_ (HM.toList props) $ \(pName, prop) ->
        let
          pVal   = propertyValue prop
          anSigs = propertyAnSignatures prop
          hashes = hashesForAttestation subj pName pVal
        in
          traverse_ (hashes `isAttestedBy`) anSigs

    validator2 :: Either Text ()
    validator2 =
      validateMetadataAttestationSignatures meta
        & Validation.toEither
        & Bifunctor.first (foldMap prettyPrintAttestationError)

  validator1 === validator2

unit_attestation_invalid_subject_fails :: Assertion
unit_attestation_invalid_subject_fails = do
  let
    subject1 = Subject "1"
    subject2 = Subject "2"
    pName    = PropertyName "1"
    pVal     = Aeson.String "1"

  skey <- mkSigningKey

  let
    anSig  = mkAnnotatedSignature skey subject1 pName pVal
    hashes = hashesForAttestation subject2 pName pVal

  hashes `isAttestedBy` anSig @?= Left "Verification failed"

unit_attestation_invalid_propertyName_fails :: Assertion
unit_attestation_invalid_propertyName_fails = do
  let
    subject = Subject "1"
    pName1  = PropertyName "1"
    pName2  = PropertyName "2"
    pVal    = Aeson.String "1"

  skey <- mkSigningKey

  let
    anSig  = mkAnnotatedSignature skey subject pName1 pVal
    hashes = hashesForAttestation subject pName2 pVal

  hashes `isAttestedBy` anSig @?= Left "Verification failed"

unit_attestation_invalid_propertyValue_fails :: Assertion
unit_attestation_invalid_propertyValue_fails = do
  let
    subject = Subject "1"
    pName   = PropertyName "1"
    pVal1   = Aeson.String "1"
    pVal2   = Aeson.String "2"

  skey <- mkSigningKey

  let
    anSig  = mkAnnotatedSignature skey subject pName pVal1
    hashes = hashesForAttestation subject pName pVal2

  hashes `isAttestedBy` anSig @?= Left "Verification failed"

unit_attestation_golden :: Assertion
unit_attestation_golden = do
  let
    metadataJSON =
      [r|{
            "subject": "3513560a0f272e96605cd88c0c892208e00781ba2403c1127c7b1da34fdbf058",
            "preImage": {
                "value": "6d792d676f6775656e2d736372697074",
                "hashFn": "sha256"
            },
            "name": {
                "value": "My Goguen Script",
                "anSignatures": [
                    {
                        "publicKey": "5de05d0693edbdd2b6d746ecada7567ec7bba1ba8e68a3a4ccc947d108bde575",
                        "signature": "b156bee69bef3eb8c24fd43d5924e436e638378e423225384755393e977cf88e2344b899fd1575896f6ddcd9ceb1635454719b10f623f6b69c7041d047cd930c"
                    }
                ]
            },
            "description": {
                "value": "A script I have registered on chain",
                "anSignatures": [
                    {
                        "publicKey": "5de05d0693edbdd2b6d746ecada7567ec7bba1ba8e68a3a4ccc947d108bde575",
                        "signature": "c0329598e4806b211a80856ce7087029bc687b0a200805b713c086cfafa85f731ebcfdca5b89d7867fec4b59355faac8a2eb243c0871617ac2bc6ef57cf98e08"
                    }
                ]
            }
        }|]

  case Aeson.eitherDecode metadataJSON of
    Left err ->
      fail err
    Right (meta :: Weakly.Metadata) -> do
      validateMetadataAttestationSignatures meta @?= Validation.Success ()



mkSigningKey :: IO (Crypto.SignKeyDSIGN Crypto.Ed25519DSIGN)
mkSigningKey = do
  seed <- Crypto.readSeedFromSystemEntropy 32
  pure $ Crypto.genKeyDSIGN seed
