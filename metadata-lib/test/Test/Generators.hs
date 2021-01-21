
module Test.Generators where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import           Data.Word
import           Hedgehog (Gen, Property, forAll, property, tripping, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog
import qualified Data.Aeson as Aeson

import Metadata.Server.Types

hashFn :: Gen HashFn
hashFn = Gen.choice [ pure Blake2b256
                    , pure Blacke2b224
                    , pure SHA256
                    ]

publicKey :: Gen Text
publicKey = Gen.text (Range.linear 0 64) Gen.hexit

sig :: Gen Text
sig = Gen.text (Range.linear 0 128) Gen.hexit

annotatedSignature :: Gen AnnotatedSignature
annotatedSignature = AnnotatedSignature <$> publicKey <*> sig

metadataValue :: Gen Text
metadataValue = Gen.text (Range.linear 0 maxBound) Gen.unicodeAll

metadataProperty :: Gen Property
metadataProperty = Property <$> metadataValue <*> Gen.list (Range.linear 0 25) annotatedSignature

preImage :: Gen PreImage
preImage = PreImage <$> metadataValue <*> hashFn

owner :: Gen Owner
owner = Owner <$> publicKey <*> sig

entry :: Gen Entry
entry = Entry <$> metadataValue <*> owner <*> metadataValue <*> metadataValue <*> preImage

-- -- | Generate random contributions.
-- --
-- -- Word8 was chosen because it is large enough to give us a decent
-- -- range of values, but small enough that generating random Word8's is
-- -- fairly likely to result in duplicate values, which are exactly the
-- -- values we are interested in testing. You are also more likely to
-- -- encounter overflow errors with such a small maximum bound.
-- contributions :: Gen (Contributions Word8 Word8 Word8)
-- contributions = Gen.recursive Gen.choice
--   [ mempty ]
--   [ Contrib.contribute <$> Gen.word8 (Range.linear 0 maxBound) <*> Gen.word8 (Range.linear 0 maxBound) <*> Gen.word8 (Range.linear 0 maxBound) <*> contributions
--   , Contrib.withdraw <$> Gen.word8 (Range.linear 0 maxBound) <*> Gen.word8 (Range.linear 0 maxBound) <*> contributions
--   , (<>) <$> contributions <*> contributions
--   ]
