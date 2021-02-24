{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Metadata.Types.Wallet
  ( tests
  ) where

import qualified Data.Aeson                       as Aeson
import qualified Data.Aeson.Types                 as Aeson
import           Data.ByteArray.Encoding          (Base (Base16, Base64),
                                                   convertToBase)
import qualified Data.ByteString                  as B
import qualified Data.HashMap.Strict              as HM
import           Data.Maybe                       (fromJust)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import           Hedgehog                         (property, tripping, (===))
import qualified Hedgehog                         as H (Property)
import           Hedgehog.Internal.Property       (forAllT)
import           Network.URI                      (parseURI)
import           Test.Tasty                       (TestTree, testGroup)
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit                 (Assertion, testCase, (@?=))
import Cardano.Api (ScriptInEra(ScriptInEra), ScriptLanguageInEra(SimpleScriptV2InMary), SlotNo(SlotNo), TimeLocksSupported(TimeLocksInSimpleScriptV2), SimpleScript(RequireTimeBefore), Script(SimpleScript), SimpleScriptVersion(SimpleScriptV2), serialiseToCBOR)

import           Test.Cardano.Helpers             (prop_json_roundtrips)
import qualified Test.Cardano.Metadata.Generators as Gen
import           Cardano.Metadata.Types.Common    (Encoded (Encoded),
                                                   Property (Property), Subject(Subject))
import           Cardano.Metadata.Types.Wallet    (AssetLogo (AssetLogo),
                                                   AssetURL (AssetURL),
                                                   AssetUnit (AssetUnit),
                                                   Ticker (Ticker),
                                                   assetLogoMaxLength, verifyPolicy, metaPolicy, metaSubject)
import qualified Cardano.Metadata.Types.Wallet    as Wallet
import qualified Cardano.Metadata.Types.Weakly    as Weakly

tests :: TestTree
tests = testGroup "Wallet Metadata type tests"
  [ testGroup "Parsers and printers"
      [ testProperty "AssetLogo/json/roundtrips" (prop_json_roundtrips Gen.assetLogo)
      , testCase "AssetLogo/json/matches-spec" unit_assetLogo_json_spec

      , testProperty "AssetURL/json/roundtrips" (prop_json_roundtrips Gen.assetURL)
      , testCase "AssetURL/json/matches-spec" unit_assetURL_json_spec

      , testProperty "AssetUnit/json/roundtrips" (prop_json_roundtrips Gen.assetUnit)
      , testCase "AssetUnit/json/matches-spec" unit_assetUnit_json_spec

      , testProperty "Ticker/json/roundtrips" (prop_json_roundtrips Gen.ticker)
      , testCase "Ticker/json/matches-spec" unit_ticker_json_spec

      , testCase "Wallet/extra-constraints/json/matches-spec" unit_extra_constraints_json_spec
      , testProperty "Wallet/Metadata/fromWeakly/roundtrips" prop_fromWeakly_roundtrips

      , testProperty "Policy/json/roundtrips" (prop_json_roundtrips Gen.policy)
      , testProperty "Policy/Wallet.Metadata/verifies" prop_wallet_metadata_policy_verifies
      ]
  ]

prop_wallet_metadata_policy_verifies :: H.Property
prop_wallet_metadata_policy_verifies = property $ do
  meta <- forAllT Gen.walletMetadata'

  verifyPolicy (metaPolicy meta) (metaSubject meta) === Right ()

unit_extra_constraints_json_spec :: Assertion
unit_extra_constraints_json_spec = do
  let badSubject1   = ""
      badSubject2   = T.pack $ take 257 $ repeat 'a'
      script        = ScriptInEra SimpleScriptV2InMary (SimpleScript SimpleScriptV2 (RequireTimeBefore TimeLocksInSimpleScriptV2 (SlotNo 5183319957624374596)))
      goodPolicyRaw = T.decodeUtf8 $ convertToBase Base16 $ serialiseToCBOR script
      goodPolicy    = Wallet.mkPolicy script
      goodSubject   = Wallet.policyId goodPolicy <> (T.pack $ take 200 $ repeat 'a')

      badName1        = ""
      badName2        = T.pack $ take 51 $ repeat 'a'
      goodName        = T.pack $ take 50 $ repeat 'a'
      badDescription  = T.pack $ take 501 $ repeat 'a'
      goodDescription = T.pack $ take 500 $ repeat 'a'

      asWeakProp a = Property (Aeson.String a) (Just [])
      asWeakMetadata subj policy name desc = Weakly.Metadata (Subject subj) (HM.fromList [("name", asWeakProp name), ("description", asWeakProp desc), ("policy", Property (Aeson.String policy) Nothing)])
      asProp a = Property a (Just [])
      fromWeakMetadataTest weakMeta expect =
        Aeson.parseEither Wallet.fromWeaklyTypedMetadata weakMeta @?= expect

  fromWeakMetadataTest
    (asWeakMetadata goodSubject goodPolicyRaw goodName goodDescription)
    (Right $ Wallet.Metadata (Subject goodSubject) goodPolicy (asProp goodName) (asProp goodDescription) Nothing Nothing Nothing Nothing mempty)
  fromWeakMetadataTest
    (asWeakMetadata goodSubject goodPolicyRaw goodName goodDescription)
    (Right $ Wallet.Metadata (Subject goodSubject) goodPolicy (asProp goodName) (asProp goodDescription) Nothing Nothing Nothing Nothing mempty)
  fromWeakMetadataTest
    (asWeakMetadata goodSubject goodPolicyRaw goodName badDescription)
    (Left "Error in $: Length must be no more than 500 characters, got 501")
  fromWeakMetadataTest
    (asWeakMetadata goodSubject goodPolicyRaw badName1 goodDescription)
    (Left "Error in $: Length must be at least 1 characters, got 0")
  fromWeakMetadataTest
    (asWeakMetadata goodSubject goodPolicyRaw badName2 goodDescription)
    (Left "Error in $: Length must be no more than 50 characters, got 51")
  fromWeakMetadataTest
    (asWeakMetadata badSubject1 goodPolicyRaw goodName goodDescription)
    (Left "Error in $: Length must be at least 1 characters, got 0")
  fromWeakMetadataTest
    (asWeakMetadata badSubject2 goodPolicyRaw goodName goodDescription)
    (Left "Error in $: Length must be no more than 256 characters, got 257")

unit_ticker_json_spec :: Assertion
unit_ticker_json_spec = do
  let goodTicker  = "ABCD"
      badTicker1  = "A"
      badTicker2  = "ABCDE"
      asJSON     = Aeson.String

  Aeson.fromJSON (asJSON goodTicker) @?= (Aeson.Success $ Ticker goodTicker)
  Aeson.fromJSON (asJSON badTicker1) @?= (Aeson.Error "Length must be at least 2 characters, got 1" :: Aeson.Result Ticker)
  Aeson.fromJSON (asJSON badTicker2) @?= (Aeson.Error "Length must be no more than 4 characters, got 5" :: Aeson.Result Ticker)

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
  a <- forAllT Gen.walletMetadata'

  tripping a Wallet.toWeaklyTypedMetadata (Aeson.parseEither Wallet.fromWeaklyTypedMetadata)
