{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Metadata.Server.Types
  ( tests
  ) where

import qualified Data.Aeson                       as Aeson
import qualified Data.HashMap.Strict              as HM
import           Test.Tasty                       (TestTree, testGroup)
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit                 (Assertion,
                                                   testCase, (@?=))
import           Text.RawString.QQ

import           Cardano.Metadata.Server.Types
import           Cardano.Metadata.Types.Common    (AnnotatedSignature (AnnotatedSignature),
                                                   Property (Property),
                                                   deserialiseAttestationSignature, deserialiseBase16, deserialisePublicKey)
import qualified Cardano.Metadata.Types.Weakly    as Weakly
import           Test.Cardano.Helpers             (prop_json_only_has_keys,
                                                   prop_json_roundtrips)
import qualified Test.Cardano.Metadata.Generators as Gen

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
                "preImage": {
                    "value": "6d792d676f6775656e2d736372697074",
                    "hashFn": "sha256"
                },
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
                [ ( "preImage"
                  , Property (Aeson.Object $ HM.fromList [("value", Aeson.String "6d792d676f6775656e2d736372697074"), ("hashFn", Aeson.String "sha256")]) Nothing
                  )
                , ( "name"
                  , Property
                     (Aeson.String "Wallet #6")
                     (Just [ either (error "Test data malformed") id $ AnnotatedSignature
                       <$> (deserialiseAttestationSignature =<< deserialiseBase16 "2e27065e365d38bef19b7bec139206f99b00effc8a2ad05bd22259aa939dd5083f25da91c4cb764eb1bfbce243ec32cce112be9762e1da7a38e975ebb0cc0b08")
                       <*> (deserialisePublicKey =<< deserialiseBase16 "44b57ee30cdb55829d0a5d4f046baef078f1e97a7f21b62d75f8e96ea139c35f")
                     ])
                  )
                ]
        )
      , ( Weakly.Metadata
            "7f71940915ea5fe85e840f843c929eba467e6f050475bad1f10b9c274d1888c0"
            $ HM.fromList
                [ ("description", Property (Aeson.String "rex")
                     (Just [ either (error "Test data malformed") id $ AnnotatedSignature
                       <$> (deserialiseAttestationSignature =<< deserialiseBase16 "7ef6ed44ba9456737ef8d2e31596fdafb66d5775ac1a254086a553b666516e5895bb0c6b7ba8bef1f6b4d9bd9253b4449d1354de2f9e043ea4eb43fd42f87108")
                       <*> (deserialisePublicKey =<< deserialiseBase16 "0ee262f062528667964782777917cd7139e19e8eb2c591767629e4200070c661")
                     ])
                  )
                ]
        )
      ]
    )
