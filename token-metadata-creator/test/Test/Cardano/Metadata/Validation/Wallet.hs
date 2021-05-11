{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Metadata.Validation.Wallet
  ( tests
  ) where

import Cardano.Metadata.Types (hashPolicy, Subject(Subject), HashesForAttestation, SequenceNumber(SequenceNumber), Name(Name), hashesForAttestation, SomeSigningKey(..), makeAttestationSignature, Description(..), Policy(..), WellKnownProperty, parseWellKnown, Decimals(..), Logo(..), Url(..), Ticker(..), unProperty, wellKnownPropertyName) 
import Cardano.Metadata.GoguenRegistry (GoguenRegistryEntry(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Scientific
import Data.Proxy
import Data.Foldable
    ( forM_, traverse_ )
import Data.Maybe (fromJust, maybeToList, fromMaybe)
import Data.Int
    ( Int32, Int64, Int8 )
import Data.Scientific (toBoundedInteger)
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Lens
import GHC.Stack (HasCallStack)
import Data.Aeson.Lens
import qualified Data.Vector as V
import Data.Aeson.Encode.Pretty
import Data.Validation
    ( Validation (Failure, Success) )
import Hedgehog
    ( Gen, MonadGen, forAll, property, (===), annotate, failure , MonadTest)
import Hedgehog.Internal.Property (forAllT)
import qualified Hedgehog as H
    ( Property )
import qualified Hedgehog.Gen as Gen
import Control.Monad.IO.Class (MonadIO)
import Hedgehog.Internal.Property
    ( forAllT )
import qualified Hedgehog.Range as Range
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import Numeric.Natural
    ( Natural )
import System.FilePath.Posix
    ( addExtension, dropExtension, takeExtension )
import Test.Tasty
    ( TestTree, testGroup )
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit
    ( Assertion, testCase, (@?=), assertFailure)
import Text.RawString.QQ
    ( r )
import Data.String
import Data.Aeson.QQ (aesonQQ)
import Cardano.Api (CardanoEra(MaryEra), ScriptInEra(ScriptInEra), scriptLanguageSupportedInEra, AnyScriptLanguage(AnyScriptLanguage), Key, VerificationKey, getVerificationKey, SlotNo(SlotNo), SigningKey(PaymentSigningKey), ScriptInEra, MaryEra, serialiseToCBOR, ScriptLanguage(SimpleScriptLanguage, PlutusScriptLanguage), Script(SimpleScript), SimpleScriptVersion, SimpleScript(RequireAllOf, RequireAnyOf, RequireMOf, RequireSignature, RequireTimeBefore, RequireTimeAfter), verificationKeyHash, AsType(AsPaymentKey), timeLocksSupported, deterministicSigningKey, deterministicSigningKeySeedSize, serialiseToRawBytesHex, ScriptLanguageInEra(SimpleScriptV1InMary), SimpleScriptVersion(SimpleScriptV1), PaymentKey)
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Seed as Crypto

import Test.Cardano.Metadata.Generators (signingKey)
import Cardano.Metadata.View.JSON
import Cardano.Metadata.View.JSON.Lens
import Cardano.Metadata.Types.Common
    ( AnnotatedSignature (AnnotatedSignature)
    , AttestedProperty (AttestedProperty)
    , File (File)
    , unSubject
    , attestedSignatures
    , attestedSequenceNumber
    , attestedValue
    , deserialiseAttestationSignature
    , deserialiseBase16
    , deserialisePublicKey
    , seqFromNatural
    , seqSucc
    , seqZero
    , unSequenceNumber
    )
import Cardano.Metadata.Validation.Rules
    ( ValidationError (ErrorCustom, ErrorMetadataFileBaseNameLengthBounds, ErrorMetadataFileExpectedExtension, ErrorMetadataFileNameDoesntMatchSubject, ErrorMetadataFileTooBig, ErrorMetadataPropertySequenceNumberMustBeLarger)
    , ValidationError_
    , defaultRules
    , apply
    , baseFileNameLengthBounds
    , isJSONFile
    , maxFileSizeBytes
    , sequenceNumber
    , sequenceNumbers
    , subjectMatchesFileName
    , toAttestedPropertyDiffs
    )
import Cardano.Metadata.Validation.Types
    ( Difference (Added, Changed, Removed)
    , Metadata (Metadata)
    , invalid
    , metaSubject
    , metaAttestedProperties
    , metaVerifiableProperties
    , onMatchingAttestedProperties
    , valid
    )
import Cardano.Metadata.Validation.Wallet

someSigningKey :: MonadIO m => m (SigningKey PaymentKey)
someSigningKey = PaymentSigningKey <$> signingKey

-- | From a JSON value, parse some Metadata and use the length of the
-- pretty encoded JSON value and parsed Subject to make a well-formed
-- File.
asMetadataFile :: Aeson.Value -> File Metadata
asMetadataFile json =
  let
    -- Prettily print the JSON to get a realistic and consistent file size
    fileSize = BSL.length (encodePretty json)
    metadata = case Aeson.fromJSON json of
      Aeson.Error err -> error err
      Aeson.Success a -> a
    fileName = unSubject (metaSubject metadata) <> ".json"
  in
    File metadata (fromIntegral fileSize) (T.unpack fileName)

signWellKnownProperties :: SomeSigningKey -> Aeson.Value -> Aeson.Value
signWellKnownProperties skey =
  signPropertyWith (unProperty (wellKnownPropertyName (Proxy @Name))) skey
  . signPropertyWith (unProperty (wellKnownPropertyName (Proxy @Description))) skey
  . signPropertyWith (unProperty (wellKnownPropertyName (Proxy @Logo))) skey
  . signPropertyWith (unProperty (wellKnownPropertyName (Proxy @Url))) skey
  . signPropertyWith (unProperty (wellKnownPropertyName (Proxy @Ticker))) skey
  . signPropertyWith (unProperty (wellKnownPropertyName (Proxy @Decimals))) skey

signPropertyWith :: Text -> SomeSigningKey -> Aeson.Value -> Aeson.Value
signPropertyWith propName (SomeSigningKey skey) v =
  case getHashesForAttestation propName v of
    Nothing -> v
    Just hashes ->
      v & _Object . at propName . non (Aeson.Object mempty) . _Object . at "signatures" ?~ Aeson.toJSON [makeAttestationSignature skey hashes]

getHashesForAttestation :: Text -> Aeson.Value -> Maybe HashesForAttestation
getHashesForAttestation propName v =
  case propName of
    x | x == unProperty (wellKnownPropertyName (Proxy @Name))
      -> hashesForWellKnown (Proxy @Name)
    x | x == unProperty (wellKnownPropertyName (Proxy @Description))
      -> hashesForWellKnown (Proxy @Description)
    x | x == unProperty (wellKnownPropertyName (Proxy @Logo))
      -> hashesForWellKnown (Proxy @Logo)
    x | x == unProperty (wellKnownPropertyName (Proxy @Url))
      -> hashesForWellKnown (Proxy @Url)
    x | x == unProperty (wellKnownPropertyName (Proxy @Ticker))
      -> hashesForWellKnown (Proxy @Ticker)
    x | x == unProperty (wellKnownPropertyName (Proxy @Decimals))
      -> hashesForWellKnown (Proxy @Decimals)
    otherwise     -> Nothing
    
  where
    hashesForWellKnown :: forall a p. WellKnownProperty p => Proxy p -> Maybe HashesForAttestation
    hashesForWellKnown _ =
      let
        propName :: Text
        propName = unProperty $ wellKnownPropertyName (Proxy @p)

        mValue :: Maybe p
        mValue = do
          val <- getPropertyValue propName v ^? _Just
          Aeson.parseMaybe parseWellKnown val
    
        mSubject :: Maybe Subject
        mSubject = Subject <$> v ^? key "subject" . _String
    
        mSequenceNumber :: Maybe SequenceNumber
        mSequenceNumber = SequenceNumber . fromIntegral <$> getSequenceNumber propName v
      in
        hashesForAttestation <$> mSubject <*> mValue <*> mSequenceNumber

addSubjectPolicy :: ScriptInEra MaryEra -> T.Text -> Aeson.Value -> Aeson.Value
addSubjectPolicy script assetName v =
  let
    policyRaw = TE.decodeUtf8 $ B16.encode $ serialiseToCBOR script
    policy = Policy policyRaw script
    policyHash = T.pack . B8.unpack . serialiseToRawBytesHex . hashPolicy $ policy
    assetNameEnc = TE.decodeUtf8 $ B16.encode $ TE.encodeUtf8 $ assetName
    subject = policyHash <> assetNameEnc
  in
    v & _Object . at "subject" ?~ Aeson.String subject
      & _Object . at "policy" ?~ Aeson.String policyRaw

addSubjectPolicy' :: SigningKey PaymentKey -> T.Text -> Aeson.Value -> Aeson.Value
addSubjectPolicy' skey =
  let
    script = ScriptInEra SimpleScriptV1InMary (SimpleScript SimpleScriptV1 (RequireSignature (verificationKeyHash . getVerificationKey $ skey)))
  in
    addSubjectPolicy script

validate :: Difference (File Metadata) -> Validation (NE.NonEmpty (ValidationError WalletValidationError)) ()
validate = apply walletValidationRules

tests :: TestTree
tests = testGroup "High-level acceptance criteria"
  [ testCase "Required fields" unit_required_fields
  , testProperty "Good decimal value" prop_decimals_field_good
  , testProperty "Bad decimal value" prop_decimals_field_bad
  , testProperty "Decimals is signed" prop_decimals_field_isSigned
  ]

shouldPass :: Difference (File Metadata) -> Assertion
shouldPass diff = validate diff @?= Success ()

propShouldPass :: (MonadTest m, HasCallStack) => Difference (File Metadata) -> m ()
propShouldPass diff = validate diff === Success ()

shouldFailWith :: ValidationError WalletValidationError -> Difference (File Metadata) -> Assertion
shouldFailWith err diff =
  case validate diff of
    Success _ -> assertFailure $ "Validating '" <> show diff <> "' should have failed, but it succeeded."
    Failure errs ->
      if err `elem` errs
      then pure ()
      else assertFailure $ "Expected error '" <> show err <> "' in errors '" <> show errs <> "'."

propShouldFail :: (MonadTest m, HasCallStack) => Difference (File Metadata) -> m ()
propShouldFail diff = case validate diff of
  Success _ -> annotate ("Validating '" <> show diff <> "' should have failed, but it succeeded.") *> failure
  Failure _ -> pure ()

propShouldFailWith :: (MonadTest m, HasCallStack) => ValidationError WalletValidationError -> Difference (File Metadata) -> m ()
propShouldFailWith err diff =
  case validate diff of
    Success _ -> annotate ("Validating '" <> show diff <> "' should have failed, but it succeeded.") *> failure
    Failure errs ->
      if err `elem` errs
      then pure ()
      else annotate ("Expected error '" <> show err <> "' in errors '" <> show errs <> "'.") *> failure

-- | Minimal metadata that will be accepted by the validator.
minimalMetadata :: SigningKey PaymentKey -> T.Text -> Aeson.Value
minimalMetadata skey assetName = [aesonQQ|
      {
          "name": {
              "value": "minimal"
          },
          "description": {
              "value": "hello world"
          }
      }
      |]
      & addSubjectPolicy' skey assetName
      & setSequenceNumbers 0

printMetadata :: File Metadata -> String
printMetadata (File meta _ _) = BSLC8.unpack . encodePretty $ meta

printFileDetails :: File Metadata -> String
printFileDetails (File _ size fileName) = "File ( _meta (" <> show size <> " bytes) (" <> fileName <> ") )"

unit_required_fields :: Assertion
unit_required_fields = do
  skey <- someSigningKey
  let
    meta = minimalMetadata skey "bucks"
      & signWellKnownProperties (SomeSigningKey skey)
      & asMetadataFile

  shouldPass $ Added meta
  shouldPass $ Changed meta meta
  shouldPass $ Removed meta

prop_decimals_field_good :: H.Property
prop_decimals_field_good = property $ do
  skey <- someSigningKey
  decimalValue <- forAll $ Gen.int (Range.constant 0 255)
  let
    meta = minimalMetadata skey "quid"
           & setPropertyValue  "decimals" (Just . Aeson.Number . fromIntegral $ decimalValue)
           & setSequenceNumber "decimals" (Just 0)
           & signWellKnownProperties (SomeSigningKey skey)
           & asMetadataFile

  annotate $ printMetadata meta
  annotate $ printFileDetails meta

  propShouldPass $ Added meta
  propShouldPass $ Changed meta meta
  propShouldPass $ Removed meta

prop_decimals_field_bad :: H.Property
prop_decimals_field_bad = property $ do
  skey <- someSigningKey
  decimalValue <- forAll $ Gen.choice [ Gen.int (Range.constant minBound (0 - 1))
                                      , Gen.int (Range.constant (255 + 1) maxBound)
                                      ]
  let
    meta = minimalMetadata skey "quid"
           & setPropertyValue  "decimals" (Just . Aeson.Number . fromIntegral $ decimalValue)
           & setSequenceNumber "decimals" (Just 0)
           & signWellKnownProperties (SomeSigningKey skey)
           & asMetadataFile

  annotate $ printMetadata meta
  annotate $ printFileDetails meta

  propShouldFailWith (ErrorCustom (WalletFailedToParseRegistryEntry "Error in $: Decimal value must be in the range [0, 255] (inclusive)")) $ Added meta
  propShouldFailWith (ErrorCustom (WalletFailedToParseRegistryEntry "Error in $: Decimal value must be in the range [0, 255] (inclusive)")) $ Changed meta meta
  propShouldPass $ Removed meta

prop_decimals_field_isSigned :: H.Property
prop_decimals_field_isSigned = property $ do
  skey <- someSigningKey
  decimalValue <- forAll $ Gen.int (Range.constant 0 255)
  let
    meta = minimalMetadata skey "quid"
           & setPropertyValue  "decimals" (Just . Aeson.Number . fromIntegral $ decimalValue)
           & setSequenceNumber "decimals" (Just 0)
           & signWellKnownProperties (SomeSigningKey skey)

  annotate (BSLC8.unpack $ encodePretty meta)

  case (meta ^? key "decimals" . key "signatures") of
    Nothing               -> annotate "decimals.signatures not present" *> failure
    Just (Aeson.Array xs) ->
      case V.toList xs of
        [] -> annotate "decimals.signatures is empty" *> failure
        xs -> pure ()
    Just (x)              -> annotate "decimals.signatures is not an array" *> failure
