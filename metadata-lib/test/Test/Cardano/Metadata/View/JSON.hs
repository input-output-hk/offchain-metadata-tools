{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Metadata.View.JSON
  ( tests
  ) where

import Data.Aeson
    ( FromJSON, ToJSON )
import qualified Data.Aeson as Aeson
import qualified Data.Bifunctor as Bifunctor
import Numeric.Natural (Natural)
import qualified Data.Vector as V
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Hedgehog
    ( MonadGen, Gen, forAll, property, (===) )
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen
import Data.Function ((&))
import qualified Hedgehog as H
    ( Property )
import Hedgehog.Internal.Property
    ( forAllT )
import Test.Tasty
    ( TestTree, testGroup )
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit
    ( Assertion, testCase, (@?=) )
import Data.Aeson.QQ (aesonQQ)

import Cardano.Metadata.View.JSON

tests :: TestTree
tests = testGroup "JSON metadata manipulator tests"
  [ testProperty "property setter creates value if not present" prop_set_missing_property_value
  , testProperty "property setter modifies value if present" prop_set_existing_property_value
  , testProperty "property setter can remove value if given Nothing" prop_remove_existing_property_value
  , testProperty "sequence number setter creates value if not present" prop_set_missing_sequence_number
  , testProperty "sequence number setter modifies value if present" prop_set_existing_sequence_number
  , testProperty "setting all sequence numbers modifies all properties present" prop_set_sequence_numbers
  , testProperty "sequence number setter can remove value if given Nothing" prop_remove_existing_sequence_number
  , testProperty "get/set laws property value" prop_get_set_laws_property_value
  ]

genPropName :: MonadGen m => m String
genPropName = Gen.string (Range.linear 0 255) Gen.unicodeAll

genPropValue :: MonadGen m => m Aeson.Value
genPropValue = Gen.recursive Gen.choice [
    -- non-recursive generators
    pure Aeson.Null
  , Aeson.Bool <$> Gen.bool
  , Aeson.Number <$> Gen.choice [ fromIntegral <$> Gen.int Range.linearBounded ]
  , Aeson.String <$> Gen.text (Range.linear 1 64) Gen.unicodeAll
  ] [
    -- recursive generators
      (Aeson.Array . V.fromList) <$> Gen.list (Range.linear 0 10) genPropValue
    , (Aeson.Object . HM.fromList) <$> Gen.list (Range.linear 0 10) ((,) <$> (T.pack <$> genPropName) <*> genPropValue)
  ]

genSequenceNumber :: MonadGen m => m Natural
genSequenceNumber = fromIntegral <$> Gen.int (Range.linear 0 (maxBound :: Int))

genAesonObject :: MonadGen m => m Aeson.Value
genAesonObject = (Aeson.Object . HM.fromList) <$> Gen.list (Range.linear 0 10) ((,) <$> (T.pack <$> genPropName) <*> genPropValue)

prop_set_missing_property_value :: H.Property
prop_set_missing_property_value = property $ do
  propName  <- forAll genPropName
  propValue <- forAll genPropValue

  let
    given = [aesonQQ|
      { }
    |]
    expected = [aesonQQ|
      {
        $propName: {
          "value": #{propValue}
        }
      }
    |]

  (given & setPropertyValue (T.pack propName) (Just propValue)) === expected

prop_set_existing_property_value :: H.Property
prop_set_existing_property_value = property $ do
  propName  <- forAll genPropName
  propValueBefore <- forAll genPropValue
  propValueAfter <- forAll genPropValue

  let
    given = [aesonQQ|
      {
        $propName: {
          "value": #{propValueBefore}
        }
      }
    |]
    expected = [aesonQQ|
      {
        $propName: {
          "value": #{propValueAfter}
        }
      }
    |]

  (given & setPropertyValue (T.pack propName) (Just propValueAfter)) === expected

prop_remove_existing_property_value :: H.Property
prop_remove_existing_property_value = property $ do
  propName  <- forAll genPropName
  propValueBefore <- forAll genPropValue

  let
    given = [aesonQQ|
      {
        $propName: {
          "value": #{propValueBefore}
        }
      }
    |]
    expected = [aesonQQ|
      {
      }
    |]

  sequenceNumber <- forAll genSequenceNumber

  let
    given2 = [aesonQQ|
      {
        $propName: {
          "value": #{propValueBefore},
          "sequenceNumber": #{sequenceNumber}
        }
      }
    |]
    expected2 = [aesonQQ|
      {
        $propName: {
          "sequenceNumber": #{sequenceNumber}
        }
      }
    |]

  (given & setPropertyValue (T.pack propName) Nothing) === expected
  (given2 & setPropertyValue (T.pack propName) Nothing) === expected2

prop_set_missing_sequence_number :: H.Property
prop_set_missing_sequence_number = property $ do
  propName <- forAll genPropName
  sequenceNumber <- forAll genSequenceNumber

  let
    given = [aesonQQ|
      { }
    |]
    expected = [aesonQQ|
      {
        $propName: {
          "sequenceNumber": #{sequenceNumber}
        }
      }
    |]

  (given & setSequenceNumber (T.pack propName) (Just sequenceNumber)) === expected

prop_set_existing_sequence_number :: H.Property
prop_set_existing_sequence_number = property $ do
  propName             <- forAll genPropName
  sequenceNumberBefore <- forAll genSequenceNumber
  sequenceNumberAfter  <- forAll genSequenceNumber

  let
    given = [aesonQQ|
      {
        $propName: {
          "sequenceNumber": #{sequenceNumberBefore}
        }
      }
    |]
    expected = [aesonQQ|
      {
        $propName: {
          "sequenceNumber": #{sequenceNumberAfter}
        }
      }
    |]

  (given & setSequenceNumber (T.pack propName) (Just sequenceNumberAfter)) === expected

prop_set_sequence_numbers :: H.Property
prop_set_sequence_numbers = property $ do
  sequenceNumberBefore <- forAll genSequenceNumber
  sequenceNumberAfter <- forAll genSequenceNumber

  let
    given = [aesonQQ|
      {
        "test": {
          "sequenceNumber": #{sequenceNumberBefore}
        },
        "other-property": {
          "value": "3"
        }
      }
    |]
    expected = [aesonQQ|
      {
        "test": {
          "sequenceNumber": #{sequenceNumberAfter}
        },
        "other-property": {
          "value": "3",
          "sequenceNumber": #{sequenceNumberAfter}
        }
      }
    |]

  (given & setSequenceNumbers sequenceNumberAfter) === expected

prop_remove_existing_sequence_number :: H.Property
prop_remove_existing_sequence_number = property $ do
  propName  <- forAll genPropName
  propValue <- forAll genPropValue
  sequenceNumber <- forAll genSequenceNumber

  let
    given = [aesonQQ|
      {
        $propName: {
          "sequenceNumber": #{sequenceNumber}
        }
      }
    |]
    expected = [aesonQQ|
      {
      }
    |]

  let
    given2 = [aesonQQ|
      {
        $propName: {
          "value": #{propValue},
          "sequenceNumber": #{sequenceNumber}
        }
      }
    |]
    expected2 = [aesonQQ|
      {
        $propName: {
          "value": #{propValue}
        }
      }
    |]

  (given & setSequenceNumber (T.pack propName) Nothing) === expected
  (given2 & setSequenceNumber (T.pack propName) Nothing) === expected2

prop_get_set_laws_property_value :: H.Property
prop_get_set_laws_property_value = property $ do
  propName <- T.pack <$> forAll genPropName
  propValue <- forAll genPropValue
  obj <- forAll genAesonObject 

  (getPropertyValue propName . setPropertyValue propName (Just propValue) $ obj) === Just propValue
  (setPropertyValue propName (Just propValue) . setPropertyValue propName (Just propValue) $ obj) === setPropertyValue propName (Just propValue) obj
  (setPropertyValue propName (getPropertyValue propName obj) obj) === obj

  sequenceNumber <- forAll genSequenceNumber

  (getSequenceNumber propName . setSequenceNumber propName (Just sequenceNumber) $ obj) === Just sequenceNumber
  (setSequenceNumber propName (Just sequenceNumber) . setSequenceNumber propName (Just sequenceNumber) $ obj) === setSequenceNumber propName (Just sequenceNumber) obj
  (setSequenceNumber propName (getSequenceNumber propName obj) obj) === obj
