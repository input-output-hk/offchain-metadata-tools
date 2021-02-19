{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Metadata.Types.Wallet
  ( tests
  ) where

import           Hedgehog (Gen, MonadTest, annotate, forAll, property, tripping, (===), footnote, failure)
import           Data.Maybe (fromJust)
import qualified Hedgehog as H (Property)
import Data.ByteArray.Encoding
    ( Base (Base16, Base64), convertFromBase, convertToBase )
import qualified Hedgehog.Gen as Gen
import qualified Data.HashMap.Strict as HM
import qualified Hedgehog.Range as Range
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HM
import Data.Aeson (ToJSON, FromJSON)
import           Test.Tasty (TestTree, testGroup)
import           Network.URI (parseURI, URI(URI))
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit (Assertion, assertEqual, testCase, (@?=))

import qualified Test.Cardano.Metadata.Generators as Gen
import Test.Cardano.Helpers (prop_json_roundtrips)

import Cardano.Metadata.Types.Common (Subject(Subject), unSubject, PropertyName, unPropertyName, asSignature, asPublicKey, Encoded(Encoded), Name, Property(Property), Description)
import Cardano.Metadata.Types.Wallet (AssetLogo(AssetLogo), AssetURL(AssetURL), AssetUnit(AssetUnit), assetLogoMaxLength, Acronym(Acronym))
import qualified Cardano.Metadata.Types.Wallet as Wallet
import qualified Cardano.Metadata.Types.Weakly as Weakly

tests :: TestTree
tests = testGroup "Wallet Metadata type tests"
  [ testGroup "Parsers and printers"
      [ testProperty "AssetLogo/json/roundtrips" (prop_json_roundtrips Gen.assetLogo)
      , testCase "AssetLogo/json/matches-spec" unit_assetLogo_json_spec

      , testProperty "AssetURL/json/roundtrips" (prop_json_roundtrips Gen.assetURL)
      , testCase "AssetURL/json/matches-spec" unit_assetURL_json_spec

      , testProperty "AssetUnit/json/roundtrips" (prop_json_roundtrips Gen.assetUnit)
      , testCase "AssetUnit/json/matches-spec" unit_assetUnit_json_spec

      , testProperty "Acronym/json/roundtrips" (prop_json_roundtrips Gen.acronym)
      , testCase "Acronym/json/matches-spec" unit_acronym_json_spec

      , testCase "Wallet/extra-constraints/json/matches-spec" unit_extra_constraints_json_spec
      , testProperty "Wallet/Metadata/fromWeakly/roundtrips" prop_fromWeakly_roundtrips
      ]
  ]

unit_extra_constraints_json_spec :: Assertion
unit_extra_constraints_json_spec = do
  let badSubject1 = ""
      badSubject2 = T.pack $ take 257 $ repeat 'a'
      goodSubject = T.pack $ take 256 $ repeat 'a'
      badName1 = ""
      badName2 = T.pack $ take 51 $ repeat 'a'
      goodName = T.pack $ take 50 $ repeat 'a'
      badDescription = T.pack $ take 501 $ repeat 'a'
      goodDescription = T.pack $ take 500 $ repeat 'a'

      asWeakProp a = Property (Aeson.String a) []
      asWeakMetadata subj name desc = Weakly.Metadata (Subject subj) (HM.fromList [("name", asWeakProp name), ("description", asWeakProp desc)])
      asProp a = Property a []
      fromWeakMetadataTest weakMeta expect =
        Aeson.parseEither Wallet.fromWeaklyTypedMetadata weakMeta @?= expect

  fromWeakMetadataTest
    (asWeakMetadata goodSubject goodName goodDescription) 
    (Right $ Wallet.Metadata (Subject goodSubject) (asProp goodName) (asProp goodDescription) Nothing Nothing Nothing Nothing mempty)
  fromWeakMetadataTest
    (asWeakMetadata goodSubject goodName badDescription) 
    (Left "Error in $: Length must be no more than 500 characters, got 501")
  fromWeakMetadataTest
    (asWeakMetadata goodSubject badName1 goodDescription) 
    (Left "Error in $: Length must be at least 1 characters, got 0")
  fromWeakMetadataTest
    (asWeakMetadata goodSubject badName2 goodDescription) 
    (Left "Error in $: Length must be no more than 50 characters, got 51")
  fromWeakMetadataTest
    (asWeakMetadata badSubject1 goodName goodDescription) 
    (Left "Error in $: Length must be at least 1 characters, got 0")
  fromWeakMetadataTest
    (asWeakMetadata badSubject2 goodName goodDescription) 
    (Left "Error in $: Length must be no more than 256 characters, got 257")

unit_acronym_json_spec :: Assertion
unit_acronym_json_spec = do
  let goodAcronym  = "ABCD"
      badAcronym1  = "A"
      badAcronym2  = "ABCDE"
      asJSON     = Aeson.String
      asAssetURL = AssetURL . fromJust . parseURI . T.unpack

  Aeson.fromJSON (asJSON goodAcronym) @?= (Aeson.Success $ Acronym goodAcronym)
  Aeson.fromJSON (asJSON badAcronym1) @?= (Aeson.Error "Length must be at least 2 characters, got 1" :: Aeson.Result Acronym)
  Aeson.fromJSON (asJSON badAcronym2) @?= (Aeson.Error "Length must be no more than 4 characters, got 5" :: Aeson.Result Acronym)

unit_assetLogo_json_spec :: Assertion
unit_assetLogo_json_spec = do
  let xs = take (assetLogoMaxLength + 1) $ repeat 0
      ys = mempty
      asJSON = Aeson.String . T.decodeUtf8 . convertToBase Base64 . B.pack
      asAssetLogo = AssetLogo . Encoded . convertToBase Base64 . B.pack

  Aeson.fromJSON (asJSON xs) @?= (Aeson.Error "Length must be no more than 65536 bytes, got 65537" :: Aeson.Result AssetLogo)
  Aeson.fromJSON (asJSON ys) @?= Aeson.Success (asAssetLogo ys)

unit_assetURL_json_spec :: Assertion
unit_assetURL_json_spec = do
  let goodURL    = "https://www.google.com"
      badURL     = "http://www.google.com"
      asJSON     = Aeson.String
      asAssetURL = AssetURL . fromJust . parseURI . T.unpack

  Aeson.fromJSON (asJSON badURL) @?= (Aeson.Error "Scheme must be https: but got http:" :: Aeson.Result AssetURL)
  Aeson.fromJSON (asJSON goodURL) @?= Aeson.Success (asAssetURL goodURL)

unit_assetUnit_json_spec :: Assertion
unit_assetUnit_json_spec = do
  let badDecimals1 = Aeson.Object $ HM.fromList $
        [ ("decimals", Aeson.Number (-1))
        , ("name", Aeson.String "steve")
        ]
      badDecimals2 = Aeson.Object $ HM.fromList $
        [ ("decimals", Aeson.Number 20)
        , ("name", Aeson.String "steve")
        ]
      badDecimals3 = Aeson.Object $ HM.fromList $
        [ ("decimals", Aeson.Number 0)
        , ("name", Aeson.String "steve")
        ]
      badName1 = Aeson.Object $ HM.fromList $
        [ ("name", Aeson.String "")
        , ("decimals", Aeson.Number 1)
        ]
      badName2 = Aeson.Object $ HM.fromList $
        [ ("name", Aeson.String $ T.pack $ take 31 $ repeat '1')
        , ("decimals", Aeson.Number 1)
        ]
      good = Aeson.Object $ HM.fromList $
        [ ("name", Aeson.String "hello")
        , ("decimals", Aeson.Number 1)
        ]

  Aeson.fromJSON badDecimals1 @?= (Aeson.Error "parsing Natural failed, unexpected negative number -1" :: Aeson.Result AssetUnit)
  Aeson.fromJSON badDecimals2 @?= (Aeson.Error "Number of decimals must be less than 19, got 20" :: Aeson.Result AssetUnit)
  Aeson.fromJSON badDecimals3 @?= (Aeson.Error "Number of decimals must be greater than 1, got 0" :: Aeson.Result AssetUnit)
  Aeson.fromJSON badName1     @?= (Aeson.Error "Length must be at least 1 characters, got 0" :: Aeson.Result AssetUnit)
  Aeson.fromJSON badName2     @?= (Aeson.Error "Length must be no more than 30 characters, got 31" :: Aeson.Result AssetUnit)
  Aeson.fromJSON good         @?= (Aeson.Success $ AssetUnit "hello" 1)

prop_fromWeakly_roundtrips :: H.Property
prop_fromWeakly_roundtrips = property $ do
  a <- forAll Gen.walletMetadata

  tripping a Wallet.toWeaklyTypedMetadata (Aeson.parseEither Wallet.fromWeaklyTypedMetadata) 
