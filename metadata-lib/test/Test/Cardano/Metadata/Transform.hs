module Test.Cardano.Metadata.Transform
  ( tests
  ) where

import Data.Validation
    ( Validation (Failure, Success) )
import Data.Word
import Hedgehog
    ( forAll, property, unOpaque, (===) )
import qualified Hedgehog as H
    ( Property )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
    ( TestTree, testGroup )
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit
    ( Assertion, testCase, (@?=) )

-- import           Test.Cardano.Helpers             (prop_functor_laws)

import Cardano.Metadata.Transform
import qualified Test.Cardano.Metadata.Generators as Gen

tests :: TestTree
tests = testGroup "Transform algebra tests"
  [ testProperty "Transform/functor/law-abiding" prop_functor_laws
  , testProperty "Transform/applicative/law-abiding" prop_applicative_laws
  , testProperty "Transform/monad/law-abiding" prop_monad_laws
  , testCase "Transform/desirable-properties" unit_desirable_properties
  ]

unit_desirable_properties :: Assertion
unit_desirable_properties = do
  -- Can make transforms
  let t1 = mkTransform (const (Left "fail"))
      t2 = mkTransform Right

  -- Combine them using the applicative instance
  let tSum = t1 *> t2

  -- And get the expected result
  tSum `apply` "dummy" @?= Left "fail"

  -- Additionally, we can do the same with Validation
  let v1 = mkTransform (const (Failure ["fail1"]))
      v2 = mkTransform (const (Failure ["fail2"]))
      v3 = mkTransform Success

  let vSum = v1 *> v2 *> v3

  vSum `apply` "dummy" @?= Failure ["fail1", "fail2"]

prop_functor_laws :: H.Property
prop_functor_laws = property $ do
  ofa <- forAll (Gen.transform Gen.eitherWord8)
  let fa = unOpaque ofa

  let obs = flip apply (4 :: Word8)

  -- Identity
  obs (fmap id fa) === obs (id fa)

  -- Composition
  let plusTwo  = (+ 2)
      mulThree = (* 3)

  obs (fmap (mulThree . plusTwo) fa) === obs ((fmap mulThree . fmap plusTwo) fa)

prop_applicative_laws :: H.Property
prop_applicative_laws = property $ do
  fa  <- unOpaque <$> forAll (Gen.transform Gen.eitherWord8)
  ffb <- unOpaque <$> forAll (Gen.transform (fmap (+) <$> Gen.eitherWord8))
  ffc <- unOpaque <$> forAll (Gen.transform (fmap (*) <$> Gen.eitherWord8))

  let obs = flip apply (4 :: Word8)

  -- Identity
  obs (pure id <*> fa) === obs (fa)

  -- Composition
  obs (pure (.) <*> ffc <*> ffb <*> fa) === obs (ffc <*> (ffb <*> fa))

  -- Homomorphism
  a <- forAll (Gen.word8 Range.constantBounded)
  b <- forAll (Gen.word8 Range.constantBounded)
  let f = (+)
  obs (pure (f a) <*> pure b) === (obs (pure (f a b) :: Transform r (Either Word8) Word8))

  -- Interchange
  y <- forAll (Gen.word8 Range.constantBounded)
  z <- forAll (Gen.word8 Range.constantBounded)
  let u = pure (+ z)
  obs (u <*> pure y) === obs (pure ($ y) <*> u :: Transform Word8 (Either Word8) Word8)

prop_monad_laws :: H.Property
prop_monad_laws = property $ do
  a <- forAll (Gen.word8 Range.constantBounded)
  let k = pure
  m <- unOpaque <$> forAll (Gen.transform Gen.eitherWord8)
  mh <- unOpaque <$> forAll (Gen.transform Gen.eitherWord8)
  let h = const mh

  let obs = flip apply (4 :: Word8)

  -- Left identity
  obs (return a >>= k) === obs (k a)

  -- Right identity
  obs (m >>= return) === obs (m)

  -- Associativity
  obs (m >>= (\x -> k x >>= h)) === obs ((m >>= k) >>= h)
