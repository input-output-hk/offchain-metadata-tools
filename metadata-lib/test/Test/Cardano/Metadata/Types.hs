{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Metadata.Types
  ( tests
  ) where

import           Data.Aeson                       (FromJSON, ToJSON)
import qualified Data.Aeson                       as Aeson
import qualified Data.Aeson                       as Aeson
import qualified Data.Aeson.Encode.Pretty         as Aeson
import qualified Data.Bifunctor                   as Bifunctor
import           Data.ByteArray.Encoding          (Base (Base16, Base64),
                                                   convertFromBase,
                                                   convertToBase)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy.Char8       as BLC
import           Data.Functor.Identity            (Identity (Identity))
import qualified Data.HashMap.Strict              as HM
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (delete, find, sort)
import           Data.Maybe                       (fromJust)
import           Data.Monoid                      (First (First), Sum (Sum),
                                                   getSum)
import           Data.Ratio                       (Ratio, (%))
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import           Data.Word                        (Word8)
import           Hedgehog                         (Gen, MonadTest, annotate,
                                                   failure, footnote, forAll,
                                                   property, tripping, (===))
import qualified Hedgehog                         as H (Property)
import qualified Hedgehog.Gen                     as Gen
import qualified Hedgehog.Range                   as Range
import           Network.URI                      (URI (URI), parseURI)
import           Test.Tasty                       (TestTree, testGroup)
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit                 (Assertion, assertEqual,
                                                   testCase, (@?=))
import           Text.RawString.QQ
import           Text.Read                        (readEither)

import           Test.Cardano.Helpers             (prop_json_only_has_keys,
                                                   prop_json_roundtrips,
                                                   prop_read_show_roundtrips)
import qualified Test.Cardano.Metadata.Generators as Gen

import           Cardano.Metadata.Types.Common    (HashFn (Blake2b224, Blake2b256, SHA256),
                                                   PropertyName, Subject, Property(Property),
                                                   asPublicKey, asSignature,
                                                   propertyAnSignatures,
                                                   propertyValue,
                                                   unPropertyName, unSubject)
import qualified Cardano.Metadata.Types.Weakly    as Weakly

tests :: TestTree
tests = testGroup "Metadata type tests"
  [ testGroup "Parsers and printers"
      [
        testProperty "Metadata/json/roundtrips" (prop_json_roundtrips Gen.weaklyTypedMetadata)
      , testProperty "Metadata/json/matches-spec" prop_json_metadata_spec

      , testProperty "Property/json/roundtrips" (prop_json_roundtrips Gen.weaklyTypedProperty)
      , testProperty "Property/json/matches-spec" prop_json_property_spec
      , testCase     "Property/json/missing-anSignatures-ok" unit_property_missing_annotatedSignatures

      , testProperty "Name/json/roundtrips" (prop_json_roundtrips Gen.name)
      , testProperty "Description/json/roundtrips" (prop_json_roundtrips Gen.description)

      , testProperty "Subject/json/roundtrips" (prop_json_roundtrips Gen.subject)
      , testProperty "Subject/json/matches-spec" prop_json_subject_spec

      , testProperty "PropertyName/json/roundtrips" (prop_json_roundtrips Gen.propertyName)
      , testProperty "PropertyName/json/matches-spec" prop_json_propertyName_spec

      , testProperty "AnnotatedSignature/json/roundtrips" (prop_json_roundtrips Gen.annotatedSignature)
      , testProperty "AnnotatedSignature/json/matches-spec" prop_json_annotatedSignature_spec

      , testProperty "PreImage/json/roundtrips" (prop_json_roundtrips Gen.preImage)
      , testProperty "PreImage/json/matches-spec-keys" (prop_json_only_has_keys Gen.preImage ["value", "hashFn"])

      , testProperty "Owner/json/roundtrips" (prop_json_roundtrips Gen.owner)
      , testProperty "Owner/json/matches-spec-keys" (prop_json_only_has_keys Gen.owner ["publicKey", "signature"])

      , testProperty "HashFn/read/show/roundtrips" (prop_read_show_roundtrips Gen.hashFn)
      , testCase     "HashFn/show/matches-spec" unit_hashfn_show_spec
      , testProperty "HashFn/json/roundtrips" (prop_json_roundtrips Gen.hashFn)
      , testProperty "HashFn/json/matches-spec-keys" (prop_json_read_show_align_spec Gen.hashFn)

      ]
  ]

-- The from/to JSON instances should simply match the show/read
-- instances.
prop_json_read_show_align_spec :: forall a. (Eq a, Show a, Read a, ToJSON a, FromJSON a) => Gen a -> H.Property
prop_json_read_show_align_spec gen = property $ do
  a <- forAll gen

  (Aeson.String $ T.pack $ show a) === (Aeson.toJSON a)
  (Aeson.eitherDecode $ BLC.pack $ "\"" <> show a <> "\"" :: Either String a) === (readEither $ show a)

unit_hashfn_show_spec :: Assertion
unit_hashfn_show_spec = do
  show Blake2b256 @?= "blake2b-256"
  show Blake2b224 @?= "blake2b-224"
  show SHA256     @?= "sha256"

unit_property_missing_annotatedSignatures :: Assertion
unit_property_missing_annotatedSignatures = do
  let
    json = [r|
      {
        "value": "string"
      }
    |]

  Aeson.eitherDecode json @?= Right (Property (Aeson.String "string") [])

prop_json_subject_spec :: H.Property
prop_json_subject_spec = property $ do
  subj <- forAll Gen.subject

  Aeson.toJSON subj === Aeson.String (unSubject subj)

prop_json_propertyName_spec :: H.Property
prop_json_propertyName_spec = property $ do
  p <- forAll Gen.propertyName

  Aeson.toJSON p === Aeson.String (unPropertyName p)

prop_json_annotatedSignature_spec :: H.Property
prop_json_annotatedSignature_spec = property $ do
  as <- forAll Gen.annotatedSignature

  Aeson.toJSON as === Aeson.Object (HM.fromList
                                     [ ("signature", Aeson.String $ asSignature as)
                                     , ("publicKey", Aeson.String $ asPublicKey as)
                                     ]
                                  )

prop_json_property_spec :: H.Property
prop_json_property_spec = property $ do
  p <- forAll Gen.weaklyTypedProperty

  Aeson.toJSON p === Aeson.Object (HM.fromList
                                     [ ("value", propertyValue p)
                                     , ("anSignatures", Aeson.toJSON $ propertyAnSignatures p)
                                     ]
                                  )

prop_json_metadata_spec :: H.Property
prop_json_metadata_spec = property $ do
  m <- forAll Gen.weaklyTypedMetadata

  Aeson.toJSON m === Aeson.Object (HM.fromList $
                                     [ ("subject", Aeson.String $ unSubject $ Weakly.metaSubject m) ]
                                     <> (fmap (Bifunctor.first unPropertyName) $ HM.toList $ fmap Aeson.toJSON $ Weakly.metaProperties m)
                                  )
