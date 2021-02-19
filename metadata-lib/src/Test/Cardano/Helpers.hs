module Test.Cardano.Helpers where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import           Data.List (sort)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Hedgehog (Gen, forAll, property, tripping, MonadTest, footnote, failure, (===))
import qualified Hedgehog as H (Property)
import Text.Read (readEither)

prop_read_show_roundtrips :: (Show a, Eq a, Read a) => Gen a -> H.Property
prop_read_show_roundtrips gen = property $ do
  a <- forAll gen

  tripping a show readEither

prop_json_roundtrips :: (Show a, Eq a, ToJSON a, FromJSON a) => Gen a -> H.Property
prop_json_roundtrips gen = property $ do
  a <- forAll gen

  tripping a Aeson.toJSON Aeson.fromJSON

prop_json_only_has_keys :: (Show a, ToJSON a) => Gen a -> [Text] -> H.Property
prop_json_only_has_keys genA keys = property $ do
  a <- forAll genA

  a `onlyHasKeys` keys

onlyHasKeys :: (MonadTest m, ToJSON a) => a -> [Text] -> m ()
onlyHasKeys a ks =
  case Aeson.toJSON a of
    Aeson.Object obj -> do
      sort (HM.keys obj) === sort ks
    x                -> do
      footnote $ "Expected Aeson Object but got: " <> show x
      failure
