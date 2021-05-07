{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Metadata
  ( tests
  ) where

import Cardano.Metadata.Types (hashPolicy, Subject(Subject), HashesForAttestation, SequenceNumber(SequenceNumber), Name(Name), hashesForAttestation, SomeSigningKey(..), makeAttestationSignature, Description(..), Policy(..))
import Cardano.Metadata.GoguenRegistry (GoguenRegistryEntry(..))
import qualified Data.Aeson as Aeson
import Data.Scientific
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

genVerificationKey :: Key keyrole => AsType keyrole -> Gen (VerificationKey keyrole)
genVerificationKey roletoken = getVerificationKey <$> genSigningKey roletoken

genSeed :: Int -> Gen Crypto.Seed
genSeed n = Crypto.mkSeedFromBytes <$> Gen.bytes (Range.singleton n)

genSigningKey :: Key keyrole => AsType keyrole -> Gen (SigningKey keyrole)
genSigningKey roletoken = do
    seed <- genSeed (fromIntegral seedSize)
    let sk = deterministicSigningKey roletoken seed
    return sk
  where
    seedSize :: Word
    seedSize = deterministicSigningKeySeedSize roletoken

genSlotNo :: Gen SlotNo
genSlotNo = SlotNo <$> Gen.word64 Range.constantBounded

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


propValue :: T.Text -> Traversal' Aeson.Value Aeson.Value
propValue propName = key propName . key "value"

propSeqNum :: T.Text -> Traversal' Aeson.Value SequenceNumber
propSeqNum propName = key propName . key "sequenceNumber" . _Number . lens (SequenceNumber . fromJust . toBoundedInteger) (\sci (SequenceNumber seq) -> fromIntegral seq)

-- sequenceNumberL :: Prism' SequenceNumber Aeson.Value 
-- sequenceNumberL :: Lens' Aeson.Value (Maybe SequenceNumber)
-- sequenceNumberL :: Lens' Aeson.Value (Maybe SequenceNumber)
-- sequenceNumberL = _Object . at "sequenceNumber" . traverse . _


signWellKnownProperties :: SomeSigningKey -> Aeson.Value -> Aeson.Value
signWellKnownProperties (SomeSigningKey skey) v =
  let
    subject = Subject $ v ^?! key "subject" . _String

    nameAttest =
      makeAttestationSignature skey
      $ hashesForAttestation
          subject
          (Name $ v ^?! propValue "name" . _String)
          (v ^?! propSeqNum "name")
    descriptionAttest =
      makeAttestationSignature skey
      $ hashesForAttestation
          subject
          (Description $ v ^?! propValue "description" . _String)
          (v ^?! propSeqNum "description")
  in
    v
      & key "name" . _Object . at "signatures" ?~ Aeson.toJSON [nameAttest]
      & key "description" . _Object . at "signatures" ?~ Aeson.toJSON [descriptionAttest]

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

genScript :: ScriptLanguage lang -> Gen (Script lang)
genScript (SimpleScriptLanguage lang) =
    SimpleScript lang <$> genSimpleScript lang

genScript (PlutusScriptLanguage lang) = case lang of {}

genSimpleScript :: SimpleScriptVersion lang -> Gen (SimpleScript lang)
genSimpleScript lang =
    genTerm
  where
    genTerm = Gen.recursive Gen.choice nonRecursive recursive

    -- Non-recursive generators
    nonRecursive =
         (RequireSignature . verificationKeyHash <$>
             genVerificationKey AsPaymentKey)

      : [ RequireTimeBefore supported <$> genSlotNo
        | supported <- maybeToList (timeLocksSupported lang) ]

     ++ [ RequireTimeAfter supported <$> genSlotNo
        | supported <- maybeToList (timeLocksSupported lang) ]

    -- Recursive generators
    recursive =
      [ RequireAllOf <$> Gen.list (Range.linear 0 10) genTerm

      , RequireAnyOf <$> Gen.list (Range.linear 0 10) genTerm

      , do ts <- Gen.list (Range.linear 0 10) genTerm
           m  <- Gen.integral (Range.constant 0 (length ts))
           return (RequireMOf m ts)
      ]

genScriptInEra :: CardanoEra era -> Gen (ScriptInEra era)
genScriptInEra era =
    Gen.choice
      [ ScriptInEra langInEra <$> genScript lang
      | AnyScriptLanguage lang <- [minBound..maxBound]
      , Just langInEra <- [scriptLanguageSupportedInEra era lang] ]

tests :: TestTree
tests = testGroup "High-level acceptance criteria"
  [ testCase "Required fields" unit_required_fields
  , testProperty "Good decimal value" prop_decimal_field_good
  , testProperty "Bad decimal value" prop_decimal_field_bad
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
      & sequenceNumbersL ?~ Aeson.Number 0

printMetadata :: File Metadata -> String
printMetadata (File meta _ _) = BSLC8.unpack . encodePretty $ meta

printFileDetails :: File Metadata -> String
printFileDetails (File _ size fileName) = "File ( _meta (" <> show size <> " bytes) (" <> fileName <> ") )"

sequenceNumberL :: T.Text -> Traversal' Aeson.Value (Maybe Aeson.Value)
sequenceNumberL propName = _Object . at propName . non (Aeson.Object mempty) . _Object . at "sequenceNumber"

propValueL :: T.Text -> Traversal' Aeson.Value (Maybe Aeson.Value)
propValueL propName = _Object . at propName . non (Aeson.Object mempty) . _Object . at "value"

sequenceNumbersL :: Traversal' Aeson.Value (Maybe Aeson.Value)
sequenceNumbersL = members . _Object . at "sequenceNumber"

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

prop_decimal_field_good :: H.Property
prop_decimal_field_good = property $ do
  skey <- someSigningKey
  decimalValue <- forAll $ Gen.integral (Range.constant 0 255)
  let
    meta = minimalMetadata skey "quid"
           & propValueL "decimal" ?~ Aeson.Number (fromIntegral decimalValue)
           & sequenceNumbersL ?~ Aeson.Number 0
           & signWellKnownProperties (SomeSigningKey skey)
           & asMetadataFile

  annotate $ printMetadata meta
  annotate $ printFileDetails meta

  propShouldPass $ Added meta
  propShouldPass $ Changed meta meta
  propShouldPass $ Removed meta

prop_decimal_field_bad :: H.Property
prop_decimal_field_bad = property $ do
  skey <- someSigningKey
  decimalValue <- forAll $ Gen.choice [ Gen.int (Range.constant minBound (0 - 1))
                                      , Gen.int (Range.constant (255 + 1) maxBound)
                                      ]
  let
    meta = minimalMetadata skey "quid"
           & propValueL "decimals" ?~ Aeson.Number (fromIntegral decimalValue)
           & sequenceNumbersL ?~ Aeson.Number 0
           & signWellKnownProperties (SomeSigningKey skey)
           & asMetadataFile

  annotate $ printMetadata meta
  annotate $ printFileDetails meta

  propShouldFailWith (ErrorCustom (WalletFailedToParseRegistryEntry "Error in $: Decimal value must be in the range [0, 255] (inclusive)")) $ Added meta
  propShouldFailWith (ErrorCustom (WalletFailedToParseRegistryEntry "Error in $: Decimal value must be in the range [0, 255] (inclusive)")) $ Changed meta meta
  propShouldPass $ Removed meta


