{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Metadata.Validation
  ( tests
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.Aeson.QQ
    ( aesonQQ )
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable
    ( forM_, traverse_ )
import Data.Int
    ( Int32, Int64, Int8 )
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Validation
    ( Validation (Failure) )
import Hedgehog
    ( MonadGen, forAll, property, (===) )
import qualified Hedgehog as H
    ( Property )
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Property
    ( forAllT )
import qualified Hedgehog.Range as Range
import Numeric.Natural
    ( Natural )
import System.FilePath.Posix
    ( addExtension, dropExtension, takeExtension )
import Test.Tasty
    ( TestTree, testGroup )
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit
    ( Assertion, testCase, (@?=) )
import Text.RawString.QQ
    ( r )

import Test.Cardano.Helpers
    ( prop_json_roundtrips )
import qualified Test.Cardano.Metadata.Generators as Gen

import Cardano.Metadata.Types.Common
    ( AnnotatedSignature (AnnotatedSignature)
    , AttestedProperty (AttestedProperty)
    , File (File)
    , Subject (Subject)
    , attestedSequenceNumber
    , attestedSignatures
    , attestedValue
    , deserialiseAttestationSignature
    , deserialiseBase16
    , deserialisePublicKey
    , seqFromNatural
    , seqSucc
    , seqZero
    , unSequenceNumber
    , unSubject
    )
import Cardano.Metadata.Validation.Rules
    ( ValidationError (ErrorMetadataFileBaseNameLengthBounds, ErrorMetadataFileExpectedExtension, ErrorMetadataFileNameDoesntMatchSubject, ErrorMetadataFileTooBig, ErrorMetadataPropertySequenceNumberMustBeLarger)
    , ValidationError_
    , apply
    , baseFileNameLengthBounds
    , defaultRules
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
    , metaAttestedProperties
    , metaSubject
    , metaVerifiableProperties
    , onMatchingAttestedProperties
    , valid
    )

tests :: TestTree
tests = testGroup "Validation tests"
  [ testGroup "Parsers and printers"
      [ testProperty "Metadata/json/roundtrips" (prop_json_roundtrips Gen.validationMetadata')
      , testCase "Metadata/json/matches-spec" unit_json_metadata_spec
      , testProperty "Metadata/onMatchingAttestedProperties" prop_on_matching_attested_properties
      ]
  , testGroup "Rules"
      [ testProperty "Validation/rules/maxFileSizeBytes" prop_rules_maxFileSizeBytes
      , testProperty "Validation/rules/subjectMatchesFileName" prop_rules_subjectMatchesFileName
      , testProperty "Validation/rules/sequenceNumber" prop_rules_sequenceNumber
      , testProperty "Validation/rules/sequenceNumbers" prop_rules_sequenceNumbers
      , testProperty "Validation/rules/isJSONFile" prop_rules_isJSONFile
      , testProperty "Validation/rules/baseFileNameLengthBounds" prop_rules_baseFileNameLengthBounds
      , testProperty "Validation/helpers/toAttestedPropertyDiffs" prop_helpers_toAttestedPropertyDiffs
      , testCase "Unknown but well-formed properties have sequence numbers validated" unit_sequence_number_of_unknown_property_validated
      ]
  ]

-- | From a JSON value, parse some Metadata and use the length of the
-- pretty encoded JSON value and parsed Subject to make a well-formed
-- File.
asMetadataFile :: Aeson.Value -> File Metadata
asMetadataFile json =
  let
    -- Prettily print the JSON to get a realistic and consistent file size
    fileSize = BSL.length (Aeson.encodePretty json)
    metadata = case Aeson.fromJSON json of
      Aeson.Error err -> error err
      Aeson.Success a -> a
    fileName = unSubject (metaSubject metadata) <> ".json"
  in
    File metadata (fromIntegral fileSize) (T.unpack fileName)

prop_rules_isJSONFile :: H.Property
prop_rules_isJSONFile = property $ do
  -- All files with the ".json" extension should pass
  let
    genFile = do
      (File a sz fp) <- Gen.file (pure ())
      pure (File a sz (dropExtension fp <> ".json"))

  diff <- forAll $ Gen.diff genFile

  isJSONFile diff
    === (valid :: Validation (NE.NonEmpty (ValidationError ())) ())

  -- All removed files should pass
  anyFile <- forAll $ Gen.file (pure ())
  isJSONFile (Removed anyFile)
    === (valid :: Validation (NE.NonEmpty (ValidationError ())) ())

  -- All files without a ".json" extension should fail
  extraExt <- forAll $ Gen.text (Range.linear 1 10) Gen.unicode
  let
    genBadFile = do
      (File a sz fp) <- Gen.file (pure ())
      pure (File a sz (fp `addExtension` (T.unpack extraExt)))

  badFile@(File _ _ fp) <- forAll genBadFile

  badDiff <- forAll $ Gen.choice [ pure $ Added badFile
                                 , pure $ Changed badFile badFile
                                 ]

  isJSONFile badDiff
    === (invalid $ ErrorMetadataFileExpectedExtension fp ".json" (T.pack $ takeExtension fp)
          :: Validation (NE.NonEmpty ValidationError_) ())

prop_rules_baseFileNameLengthBounds :: H.Property
prop_rules_baseFileNameLengthBounds = property $ do
  minSize <- forAll $ Gen.integral (Range.linear 1 (fromIntegral (maxBound :: Int8)))
  maxSize <- forAll $ Gen.integral (Range.linear minSize (fromIntegral (maxBound :: Int8)))

  -- Any size within the length bounds should pass
  sz <- forAll $ Gen.integral (Range.linear minSize maxSize)
  fp <- forAll $ Gen.text (Range.constant (fromIntegral sz) (fromIntegral sz)) Gen.alphaNum

  let file = File () sz (T.unpack fp)
  diff <- forAll $ Gen.diff (pure file)

  baseFileNameLengthBounds minSize maxSize diff
    === (valid :: Validation (NE.NonEmpty (ValidationError ())) ())

  -- Any size outside the length bounds should fail
  szBad <- forAll $
     Gen.choice [ Gen.integral (Range.linear (maxSize + 1) (fromIntegral (maxBound :: Int8)))
                , Gen.integral (Range.linear (minSize - 1) 0)
                ]
  fpBad <- forAll $ Gen.text (Range.constant (fromIntegral szBad) (fromIntegral szBad)) Gen.alphaNum
  let fileBad = File () sz (T.unpack fpBad)

  diffBad <- forAll $ Gen.choice [ pure $ Added fileBad
                                 , pure $ Changed fileBad fileBad
                                 ]

  baseFileNameLengthBounds minSize maxSize diffBad
    === (invalid $ ErrorMetadataFileBaseNameLengthBounds fpBad (minSize, maxSize) szBad
          :: Validation (NE.NonEmpty ValidationError_) ())

  -- All removed files should pass
  diffAny <- forAll $ Gen.choice [ pure $ Removed file
                                 , pure $ Removed fileBad
                                 ]

  baseFileNameLengthBounds minSize maxSize diffAny
    === (valid :: Validation (NE.NonEmpty ValidationError_) ())

prop_rules_maxFileSizeBytes :: H.Property
prop_rules_maxFileSizeBytes = property $ do
  let
    sizeLimit   = 2

    fWithSize :: Natural -> File ()
    fWithSize s = File () s ""

    asDiff :: MonadGen m => File a -> m (Difference (File a))
    asDiff f = Gen.choice [ Added <$> pure f
                          , Changed <$> pure f <*> pure f
                          ]

  -- A removed file of any size (even very large) is accepted
  anySize <- forAll $ Gen.integral (Range.exponential 0 (fromIntegral (maxBound :: Int64)))
  let removed = Removed $ fWithSize anySize
  maxFileSizeBytes 3 removed
    === (valid :: Validation (NE.NonEmpty (ValidationError ())) ())

  -- Any file size lower than the limit or at the limit is accepted
  lowerSize <- forAll $ Gen.integral (Range.exponential 0 sizeLimit)
  dLower  <- forAll $ asDiff $ fWithSize lowerSize
  maxFileSizeBytes sizeLimit dLower
    === (valid :: Validation (NE.NonEmpty (ValidationError ())) ())

  -- File size higher than the limit is not accepted
  higherSize <- forAll $ Gen.integral (Range.exponential (sizeLimit + 1) (fromIntegral (maxBound :: Int64)))
  dHigher <- forAll $ asDiff $ fWithSize higherSize
  maxFileSizeBytes sizeLimit dHigher
    === (Failure (ErrorMetadataFileTooBig sizeLimit higherSize NE.:| []) :: Validation (NE.NonEmpty (ValidationError ())) ())

prop_rules_subjectMatchesFileName :: H.Property
prop_rules_subjectMatchesFileName = property $ do
  anySize     <- forAll $ Gen.integral (Range.exponential 0 (fromIntegral (maxBound :: Int64)))
  anyMeta     <- forAllT $ Gen.validationMetadata'
  anyFileName <- forAll $ Gen.string (Range.linear 0 64) Gen.unicode

  -- A removed file is always accepted as valid
  let removed = Removed $ File anyMeta anySize anyFileName
  subjectMatchesFileName removed
    === (valid :: Validation (NE.NonEmpty (ValidationError ())) ())

  -- If a file is added or changed, file name must match subject
  let
    filePath = "my_dir/a.json"

    goodMeta = Metadata (Subject "a") (metaAttestedProperties anyMeta) (metaVerifiableProperties anyMeta)
    badMeta  = Metadata (Subject "b") (metaAttestedProperties anyMeta) (metaVerifiableProperties anyMeta)

    asDiff :: MonadGen m => File a -> m (Difference (File a))
    asDiff f = Gen.choice [ Added <$> pure f
                          , Changed <$> pure f <*> pure f
                          ]

  badDiff  <- forAll $ asDiff (File badMeta anySize filePath)
  subjectMatchesFileName badDiff
    === (Failure (ErrorMetadataFileNameDoesntMatchSubject (Subject "b") "a" NE.:| []) :: Validation (NE.NonEmpty (ValidationError ())) ())

  goodDiff <- forAll $ asDiff (File goodMeta anySize filePath)
  subjectMatchesFileName goodDiff
    === (valid :: Validation (NE.NonEmpty (ValidationError ())) ())

unit_sequence_number_of_unknown_property_validated :: Assertion
unit_sequence_number_of_unknown_property_validated = do
  let
    before = [aesonQQ|
                  {
                    "subject": "1234",
                    "prop": {
                      "value": "hello",
                      "sequenceNumber": 0
                    }
                  }
                  |]
    after = [aesonQQ|
                  {
                    "subject": "1234",
                    "prop": {
                      "value": "goodbye",
                      "sequenceNumber": 0
                    }
                  }
                  |]
    diff = Changed (asMetadataFile before) (asMetadataFile after)
  
  defaultRules `apply` diff
   @?= (Failure (ErrorMetadataPropertySequenceNumberMustBeLarger (AttestedProperty {attestedValue = Aeson.String "hello", attestedSignatures = [], attestedSequenceNumber = seqFromNatural 0}) (AttestedProperty {attestedValue = Aeson.String "goodbye", attestedSignatures = [], attestedSequenceNumber = seqFromNatural 0}) (seqFromNatural 0) (seqFromNatural 0) NE.:| []) :: Validation (NE.NonEmpty (ValidationError ())) ())

prop_rules_sequenceNumber :: H.Property
prop_rules_sequenceNumber = property $ do
  -- Properties added or removed are always valid
  prop <- forAllT $ Gen.attestedProperty'

  sequenceNumber (Added prop)
    === (valid :: Validation (NE.NonEmpty (ValidationError ())) ())

  sequenceNumber (Removed prop)
    === (valid :: Validation (NE.NonEmpty (ValidationError ())) ())

  -- Properties changed must have a larger sequence number
  seqOrig    <- forAll $ seqFromNatural <$> Gen.integral (Range.linear 1 (fromIntegral (maxBound :: Int32)))
  seqLarger  <- forAll $ seqFromNatural <$> Gen.integral (Range.linear (unSequenceNumber seqOrig + 1) (fromIntegral (maxBound :: Int32)))
  seqSmaller <- forAll $ seqFromNatural <$> Gen.integral (Range.linear 0 (unSequenceNumber seqOrig))

  propOrig <- forAllT $ (\(AttestedProperty v sigs _) -> AttestedProperty v sigs seqOrig) <$> Gen.attestedProperty'

  newSig <- forAllT $ Gen.annotatedSignature'
  let
    propChanged sequenceNum = AttestedProperty (attestedValue prop) (newSig:(attestedSignatures prop)) sequenceNum

  sequenceNumber (Changed propOrig (propChanged seqOrig))
    === (Failure ((ErrorMetadataPropertySequenceNumberMustBeLarger propOrig (propChanged seqOrig) seqOrig seqOrig) NE.:| []) :: Validation (NE.NonEmpty (ValidationError ())) ())

  sequenceNumber (Changed propOrig (propChanged seqLarger))
    === (valid :: Validation (NE.NonEmpty (ValidationError ())) ())

  sequenceNumber (Changed propOrig (propChanged seqSmaller))
    === (Failure ((ErrorMetadataPropertySequenceNumberMustBeLarger propOrig (propChanged seqSmaller) seqOrig seqSmaller) NE.:| []) :: Validation (NE.NonEmpty (ValidationError ())) ())

prop_rules_sequenceNumbers :: H.Property
prop_rules_sequenceNumbers = property $ do
  meta <- forAllT $ Gen.validationMetadata'
  diff <- forAll $ Gen.diff (Gen.file $ pure meta)

  sequenceNumbers diff
    === ((traverse_ sequenceNumber . toAttestedPropertyDiffs) diff :: Validation (NE.NonEmpty (ValidationError ())) ())

prop_helpers_toAttestedPropertyDiffs :: H.Property
prop_helpers_toAttestedPropertyDiffs = property $ do
  anyFilePath        <- forAll $ Gen.string (Range.linear 0 64) Gen.unicode
  anySize            <- forAll $ Gen.integral (Range.exponential 0 (fromIntegral (maxBound :: Int64)))
  anySubject         <- forAll $ Subject <$> Gen.text (Range.linear 0 64) Gen.unicode
  anyVerifiableProps <- forAll $ Gen.map (Range.linear 0 5) ((,) <$> Gen.propertyName <*> Gen.verifiableProperty)

  attestedProps      <- forAllT $ Gen.map (Range.linear 0 5) ((,) <$> Gen.propertyName <*> Gen.attestedProperty')

  -- Removing a file removes all attested properties
  toAttestedPropertyDiffs
    (Removed $ File (Metadata anySubject attestedProps anyVerifiableProps) anySize anyFilePath)
      === foldMap (pure . Removed) attestedProps

  -- Adding a file adds all attested properties
  toAttestedPropertyDiffs
    (Added $ File (Metadata anySubject attestedProps anyVerifiableProps) anySize anyFilePath)
      === foldMap (pure . Added) attestedProps

  -- Changing a file but changing no attested properties results in no changes
  let
    metaOriginal = Metadata anySubject attestedProps anyVerifiableProps
  toAttestedPropertyDiffs
    (Changed (File metaOriginal anySize anyFilePath) (File metaOriginal (anySize + 1) anyFilePath))
      === []

  -- Adding an attested prop generates an "Added" diff
  let
    newAttestedProp = AttestedProperty Aeson.Null [] seqZero
    metaAdded = Metadata anySubject (M.singleton "newprop" newAttestedProp <> attestedProps) anyVerifiableProps
  toAttestedPropertyDiffs
    (Changed (File metaOriginal anySize anyFilePath) (File metaAdded anySize anyFilePath))
      === [Added newAttestedProp]

  -- Removing an attested prop generates an "Removed" diff
  props <- forAllT $ Gen.map (Range.linear 1 5) ((,) <$> Gen.propertyName <*> Gen.attestedProperty')
  let
    props' = M.deleteAt 0 props
    removedProp = snd $ head $ M.toList props
    metaBeforeRemoval = Metadata anySubject props anyVerifiableProps
    metaAfterRemoval = Metadata anySubject props' anyVerifiableProps

  toAttestedPropertyDiffs
    (Changed (File metaBeforeRemoval anySize anyFilePath) (File metaAfterRemoval anySize anyFilePath))
      === [Removed removedProp]

  -- Changing attested properties in a file changes attested properties
  let
    propToChange = snd $ M.elemAt 0 props
    propsUpdated = M.updateAt (\_k (AttestedProperty v sigs seqNum) -> Just $ AttestedProperty v sigs (seqSucc seqNum)) 0 props
    propChanged  = snd $ M.elemAt 0 propsUpdated

    metaBeforeChange = Metadata anySubject props anyVerifiableProps
    metaAfterChange  = Metadata anySubject propsUpdated anyVerifiableProps

  toAttestedPropertyDiffs
    (Changed (File metaBeforeChange anySize anyFilePath) (File metaAfterChange anySize anyFilePath))
      === [Changed propToChange propChanged]

unit_json_metadata_spec :: Assertion
unit_json_metadata_spec = do
  let
    json = [r|
      {
        "subject": "1234",
        "url": {
            "sequenceNumber": 0,
            "value": "https://www.johnscoin.com/crypto/",
            "signatures": [
              {
                  "signature": "2e27065e365d38bef19b7bec139206f99b00effc8a2ad05bd22259aa939dd5083f25da91c4cb764eb1bfbce243ec32cce112be9762e1da7a38e975ebb0cc0b08",
                  "publicKey": "44b57ee30cdb55829d0a5d4f046baef078f1e97a7f21b62d75f8e96ea139c35f"
              }
            ]
        },
        "ticker": {
             "sequenceNumber": 10,
             "value": "ROCK"
        },
        "policy": "93119200581c435b1ccf262013a425b0d9a05410182758bf957dfa2377a6f0ed2689"
      }
    |]

    signature1 = (either (error "Test data malformed") id $ AnnotatedSignature
                   <$> (deserialiseAttestationSignature =<< deserialiseBase16 "2e27065e365d38bef19b7bec139206f99b00effc8a2ad05bd22259aa939dd5083f25da91c4cb764eb1bfbce243ec32cce112be9762e1da7a38e975ebb0cc0b08")
                   <*> (deserialisePublicKey =<< deserialiseBase16 "44b57ee30cdb55829d0a5d4f046baef078f1e97a7f21b62d75f8e96ea139c35f")
                 )

  Aeson.eitherDecode json
    @?= Right ( Metadata
                  (Subject "1234")
                  (M.fromList
                    [ ("url", AttestedProperty (Aeson.String "https://www.johnscoin.com/crypto/") [signature1] seqZero)
                    , ("ticker", AttestedProperty (Aeson.String "ROCK") [] (seqFromNatural 10))
                    ]
                  )
                  (M.singleton "policy" (Aeson.String "93119200581c435b1ccf262013a425b0d9a05410182758bf957dfa2377a6f0ed2689"))
              )

-- | onMatchingAttestedProperties correctly matches properties with
-- the same name.
prop_on_matching_attested_properties :: H.Property
prop_on_matching_attested_properties = property $ do
  s1 <- forAll Gen.subject
  s2 <- forAll Gen.subject

  verifiable1 <- forAll $ Gen.map (Range.linear 0 5) ((,) <$> Gen.propertyName <*> Gen.verifiableProperty)
  verifiable2 <- forAll $ Gen.map (Range.linear 0 5) ((,) <$> Gen.propertyName <*> Gen.verifiableProperty)

  attested <- forAllT $ Gen.map (Range.linear 0 5) ((,) <$> Gen.propertyName <*> Gen.attestedProperty')

  let meta1 = Metadata s1 attested verifiable1
      meta2 = Metadata s2 attested verifiable2

  let
    result = onMatchingAttestedProperties meta1 meta2 $ \oldValue newValue ->
          [(oldValue, newValue)]

  forM_ result $ \(old, new) ->
    old === new
