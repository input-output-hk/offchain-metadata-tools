module Test.Cardano.Metadata.Server.Types
  ( tests
  ) where

import           Data.List (delete, find)
import           Data.Monoid (Sum (Sum), getSum)
import           Data.Ratio (Ratio, (%))
import           Data.Word (Word8)
import           Hedgehog (Gen, MonadTest, Property, annotate, forAll, property, tripping, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit (Assertion, assertEqual, testCase, (@?=))

import qualified Test.Generators as Gen

tests :: TestTree
tests = testGroup "Metadata type tests" []
  [ testGroup "Parsers and printers"
      [ testProperty "HashFn/read/show/roundtrips" (prop_read_show_roundtrips Gen.hashFn)
      , testCase "HashFn/show/matches-spec" prop_hashfn_show_spec
      , testProperty "Entry/json/matches-spec" prop_metadata_entry_json_spec
      , testProperty "Entry/json/roundtrips" prop_json_roundtrips (Gen.entry)
      ]
  ]
  -- , testGroup "Unit tests"
  --     [ testCase "contribute adds a contribution" unit_contribution_adds
  --     , testCase "proportionalize correctly assigns proportions" unit_proportionalize
  --     , testCase "contributionsBy correctly finds contributions" unit_contributionsBy
  --     , testCase "contributionsFor correctly finds contributions" unit_contributionsFor
  --     , testCase "causeSumAmounts correctly sums cause amounts" unit_causeSumAmounts
  --     ]
  -- ]

prop_read_show_roundtrips :: Gen a -> Property
prop_read_show_roundtrips gen = property $ do
  a <- forAll gen

  tripping a show readEither

prop_json_roundtrips :: Gen a -> Property
prop_json_roundtrips gen = property $ do
  a <- forAll gen

  tripping a Aeson.toJSON Aeson.fromJSON

unit_hashfn_show_spec :: Assertion
unit_hashfn_show_spec = do
  show Blake2b256 @?= "blake2b-256"
  show Blake2b224 @?= "blake2b-224"
  show SHA256     @?= "sha256"

prop_json_only_has_keys Gen.preimage           ["value", "hashFn"]
prop_json_only_has_keys Gen.owner              ["publicKey", "signature"]
prop_json_only_has_keys Gen.annotatedSignature ["publicKey", "signature"]
prop_json_only_has_keys Gen.entry              ["description", "name", "owner", "preImage", "subject"]
prop_json_only_has_keys Gen.metadataProperty   ["value", "anSignatures"]

prop_json_only_has_keys :: _ -> [Text] -> Assertion
prop_json_only_has_keys genA keys = property $ do
  a <- forAll genA

  a `onlyHasKeys` keys
  
assertOnlyKeys :: Aeson.ToJSON a => a -> [Text] -> _
assertOnlyKeys a ks =
  case Aeson.toJSON a of
    Aeson.Object obj -> do
      sort (HM.keys obj) === sort ks
    x                -> do
      footnote $ "Expected Aeson Object but got: " <> show x
      failure
