{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Cardano.Metadata.Server.Types
  ( tests
  ) where

import           Data.List (delete, find, sort)
import           Data.Functor.Identity (Identity(Identity))
import           Data.Monoid (Sum (Sum), getSum, First(First))
import           Data.Ratio (Ratio, (%))
import           Data.Word (Word8)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import Data.ByteArray.Encoding
    ( Base (Base16, Base64), convertFromBase, convertToBase )
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as Aeson
import           Text.Read (readEither)
import           Text.RawString.QQ
import           qualified Data.HashMap.Strict as HM
import           Hedgehog (Gen, MonadTest, annotate, forAll, property, tripping, (===), footnote, failure)
import qualified Hedgehog as H (Property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.Aeson (ToJSON, FromJSON)
import           Test.Tasty (TestTree, testGroup)
import           Network.URI (parseURI, URI(URI))
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit (Assertion, assertEqual, testCase, (@?=))

import qualified Test.Cardano.Metadata.Generators as Gen
import Test.Cardano.Helpers (prop_read_show_roundtrips, prop_json_roundtrips, prop_json_only_has_keys)
import Cardano.Metadata.Server.Types
import Cardano.Metadata.Types.Common (Subject(Subject), Property(Property), AnnotatedSignature(AnnotatedSignature))
import qualified Cardano.Metadata.Types.Weakly as Weakly

tests :: TestTree
tests = testGroup "Server type tests"
  [ testGroup "Parsers and printers"
      [ testProperty "BatchRequest/json/roundtrips" (prop_json_roundtrips Gen.batchRequest)
      , testProperty "BatchRequest/json/matches-spec-keys" (prop_json_only_has_keys Gen.batchRequest ["subjects", "properties"])
      , testCase     "BatchRequest/json/matches-spec" unit_batchRequest_json_spec

      , testProperty "BatchResponse/json/roundtrips" (prop_json_roundtrips Gen.batchResponse)
      , testCase     "BatchResponse/json/matches-spec" unit_batch_response_json_spec
      ]
  ]

unit_subject_json_spec :: Assertion
unit_subject_json_spec = do
  let badSubject1 = Aeson.String ""
      badSubject2 = Aeson.String $ T.pack $ take 257 $ repeat 'a'
      goodSubject = Aeson.String $ T.pack $ take 256 $ repeat 'a'

  Aeson.fromJSON badSubject1 @?= (Aeson.Error "Length must be at least 1 characters, got 0" :: Aeson.Result Subject)
  Aeson.fromJSON badSubject2 @?= (Aeson.Error "Length must be no more than 256 characters, got 257" :: Aeson.Result Subject)
  Aeson.fromJSON goodSubject @?= (Aeson.Success $ Subject $ T.pack $ take 256 $ repeat 'a')

unit_batchRequest_json_spec :: Assertion
unit_batchRequest_json_spec = do
  let jsonNoProperties = [r|
  {
    "subjects": [
      "7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0"
    ]
  }
  |]

  Aeson.eitherDecode jsonNoProperties
    @?= (Right $ BatchRequest ["7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0"] Nothing)

  let jsonProperties = [r|
  {
    "subjects": [
      "7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0"
    ],
    "properties": [
      "owner",
      "description",
      "bad"
    ]
  }
  |]

  Aeson.eitherDecode jsonProperties
    @?= (Right $ BatchRequest ["7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0"] (Just ["owner", "description", "bad"]))

  let jsonPropertiesEmpty = [r|
  {
    "subjects": [
      "7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0"
    ],
    "properties": []
  }
  |]

  Aeson.eitherDecode jsonPropertiesEmpty
    @?= (Right $ BatchRequest ["7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0"] (Just []))

unit_batch_response_json_spec :: Assertion
unit_batch_response_json_spec = do
  let
    expected = [r|
      {
        "subjects": [
            {
                "subject": "44b57ee30cdb55829d0a5d4f046baef078f1e97a7f21b62d75f8e96ea139c35f",
                "name": {
                    "value": "Wallet #6",
                    "anSignatures": [
                        {
                            "signature": "2e27065e365d38bef19b7bec139206f99b00effc8a2ad05bd22259aa939dd5083f25da91c4cb764eb1bfbce243ec32cce112be9762e1da7a38e975ebb0cc0b08",
                            "publicKey": "44b57ee30cdb55829d0a5d4f046baef078f1e97a7f21b62d75f8e96ea139c35f"
                        }
                    ]
                }
            },
            {
                "subject": "7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0",
                "description": {
                    "value": "rex",
                    "anSignatures": [
                        {
                            "signature": "7ef6ed44ba9456737ef8d2e31596fdafb66d5775ac1a254086a553b666516e5895bb0c6b7ba8bef1f6b4d9bd9253b4449d1354de2f9e043ea4eb43fd42f87108",
                            "publicKey": "0ee262f062528667964782777917cd7139e19e8eb2c591767629e4200070c661"
                        }
                    ]
                }
            }
        ]
      }
  |]

  (Aeson.eitherDecode expected :: Either String BatchResponse)
    @?=
    (Right $ BatchResponse
      [ ( Weakly.Metadata
            "44b57ee30cdb55829d0a5d4f046baef078f1e97a7f21b62d75f8e96ea139c35f"
            $ HM.fromList
                [("name", Property (Aeson.String "Wallet #6") [AnnotatedSignature "2e27065e365d38bef19b7bec139206f99b00effc8a2ad05bd22259aa939dd5083f25da91c4cb764eb1bfbce243ec32cce112be9762e1da7a38e975ebb0cc0b08" "44b57ee30cdb55829d0a5d4f046baef078f1e97a7f21b62d75f8e96ea139c35f"])]
        )
      , ( Weakly.Metadata
            "7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0"
            $ HM.fromList
                [ ("description", Property (Aeson.String "rex") [AnnotatedSignature "7ef6ed44ba9456737ef8d2e31596fdafb66d5775ac1a254086a553b666516e5895bb0c6b7ba8bef1f6b4d9bd9253b4449d1354de2f9e043ea4eb43fd42f87108" "0ee262f062528667964782777917cd7139e19e8eb2c591767629e4200070c661"]) ]
        )
      ]
    )
