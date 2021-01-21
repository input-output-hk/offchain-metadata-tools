module Test.Cardano.Metadata
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
tests = testGroup "Metadata" []
  -- [ testGroup "Laws"
  --     [ testProperty "semigroup/associativity" prop_contributions_semigroup_associativity
  --     , testProperty "monoid/identity"         prop_contributions_monoid_identity
  --     , testProperty "monoid/concatenation"    prop_contributions_monoid_concat
  --     , testProperty "contribute/last-wins"    prop_contributions_contribute_lastwins
  --     , testProperty "contribute/withdraw"     prop_contributions_contribute_withdraw
  --     , testProperty "withdraw/idempotent"     prop_contributions_withdraw_idempotent
  --     , testProperty "sumAmounts/denotation"   prop_contributions_denotation_sumAmounts
  --     , testProperty "sumAmounts/contribute"   prop_contributions_sumAmounts_contribute
  --     , testProperty "sumAmounts/withdraw "    prop_contributions_sumAmounts_withdraw
  --     , testProperty "proportionalize/denotation" prop_contributions_denotation_proportionalize
  --     , testProperty "proportionalize/sums-to-one" prop_contributions_proportionalize_sumOne
  --     , testProperty "contributionsBy/denotation" prop_contributions_denotation_contributionsBy
  --     , testProperty "contributionsFor/denotation" prop_contributions_denotation_contributionsFor
  --     , testProperty "causeSumAmounts/denotation" prop_contributions_denotation_causeSumAmounts
  --     ]
  -- , testGroup "Unit tests"
  --     [ testCase "contribute adds a contribution" unit_contribution_adds
  --     , testCase "proportionalize correctly assigns proportions" unit_proportionalize
  --     , testCase "contributionsBy correctly finds contributions" unit_contributionsBy
  --     , testCase "contributionsFor correctly finds contributions" unit_contributionsFor
  --     , testCase "causeSumAmounts correctly sums cause amounts" unit_causeSumAmounts
  --     ]
  -- ]

-- unit_contribution_adds :: Assertion
-- unit_contribution_adds = do
--    (contributions $ contribute 1 1 3 mempty) @?= [(1, [(1, 3)])]
--    (contributions $ contribute 1 1 4 $ contribute 1 2 4 mempty) @?= [(1, [(1, 4), (2, 4)])]

-- unit_proportionalize :: Assertion
-- unit_proportionalize = do
--   proportionalize mempty
--     @?= ([] :: [(Word8, [(Word8, Ratio Integer)])])
--   (proportionalize $ contribute 0 0 0 mempty)
--     @?= ([(0, [(0, 0)])])
--   (proportionalize $ contribute 0 0 1 $ contribute 0 1 1 mempty)
--     @?= [(0, [(0, 1 % 2), (1, 1 % 2)])]
--   (proportionalize $ contribute 0 0 1 $ contribute 1 1 1 mempty)
--     @?= [(0, [(0, 1 % 2)]), (1, [(1, 1 % 2)])]
--   (proportionalize $ contribute 0 0 2 $ contribute 1 1 3 $ contribute 1 2 5 $ mempty)
--     @?= [(0, [(0, 2 % 10)]), (1, [(1, 3 % 10), (2, 5 % 10)])]

-- unit_contributionsBy :: Assertion
-- unit_contributionsBy = do
--    (contributionsBy 0 $ contribute 1 0 2 (contribute 0 0 1 mempty))
--      @?= [(0, 1), (1, 2)]
--    (contributionsBy 0 $ contribute 1 1 4 $ contribute 1 0 2 (contribute 0 0 1 mempty))
--      @?= [(0, 1), (1, 2)]
--    (contributionsBy 1 $ contribute 1 1 4 $ contribute 1 0 2 (contribute 0 0 1 mempty))
--      @?= [(1, 4)]

-- unit_contributionsFor :: Assertion
-- unit_contributionsFor = do
--    (contributionsFor 0 $ contribute 1 0 2 (contribute 0 0 1 mempty))
--      @?= [(0, 1)]

-- unit_causeSumAmounts :: Assertion
-- unit_causeSumAmounts = do
--    (causeSumAmounts $ contribute 1 0 2 (contribute 0 0 1 mempty))
--      @?= [(0, 1), (1, 2)]
--    (causeSumAmounts $ contribute 1 0 255 (contribute 1 1 255 mempty))
--      @?= [(1, 510)]

-- prop_contributions_semigroup_associativity :: Property
-- prop_contributions_semigroup_associativity = property $ do
--   x <- forAll Gen.contributions
--   y <- forAll Gen.contributions
--   z <- forAll Gen.contributions

--   contributions (x <> (y <> z)) === contributions ((x <> y) <> z)

-- prop_contributions_monoid_identity :: Property
-- prop_contributions_monoid_identity = property $ do
--   x <- forAll Gen.contributions

--   -- Right identity
--   x <> mempty === x
--   -- Left identity
--   mempty <> x === x

-- prop_contributions_monoid_concat :: Property
-- prop_contributions_monoid_concat = property $ do
--   xs <- forAll (Gen.list (Range.linear 0 20) Gen.contributions)

--   mconcat xs === foldr (<>) mempty xs

-- prop_contributions_contribute_lastwins :: Property
-- prop_contributions_contribute_lastwins = property $ do
--   cause <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   ident <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   amt1  <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   amt2  <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   cs    <- forAll Gen.contributions

--   contribute cause ident amt2 (contribute cause ident amt2 cs) === contribute cause ident amt2 cs

-- prop_contributions_contribute_withdraw :: Property
-- prop_contributions_contribute_withdraw = property $ do
--   cause <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   ident <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   amt   <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   -- forall contributions for which we have not already contributed.
--   cs    <- withdraw cause ident <$> forAll Gen.contributions

--   withdraw cause ident (contribute cause ident amt cs) === cs

-- prop_contributions_withdraw_idempotent :: Property
-- prop_contributions_withdraw_idempotent = property $ do
--   cause <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   ident <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   amt   <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   cs    <- forAll Gen.contributions

--   withdraw cause ident (contribute cause ident amt cs) === withdraw cause ident cs

-- prop_contributions_denotation_sumAmounts :: Property
-- prop_contributions_denotation_sumAmounts = property $ do
--   cs    <- forAll Gen.contributions

--   sumAmounts cs
--     === ( getSum
--         . foldMap (foldMap (foldMap (foldMap (Sum . toRational))))
--         . contributions $ cs
--         )

-- prop_contributions_sumAmounts_contribute :: Property
-- prop_contributions_sumAmounts_contribute = property $ do
--   cause <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   ident <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   amt   <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   -- forall contributions for which we have not already contributed.
--   cs    <- withdraw cause ident <$> forAll Gen.contributions

--   let
--     cs' = contribute cause ident amt cs

--   sumAmounts cs' === sumAmounts cs + (toRational amt)

-- prop_contributions_sumAmounts_withdraw :: Property
-- prop_contributions_sumAmounts_withdraw = property $ do
--   cause <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   ident <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   amt   <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   -- forall contributions for which we have contributed
--   cs    <- contribute cause ident amt <$> forAll Gen.contributions

--   let
--     cs' = withdraw cause ident cs

--   sumAmounts cs' === sumAmounts cs - (toRational amt)

-- prop_contributions_denotation_proportionalize :: Property
-- prop_contributions_denotation_proportionalize = property $ do
--   cause <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   ident <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   amt   <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   -- forall contributions for which there is at least 1 contribution > 0
--   cs    <-
--     contribute cause ident (if amt == 0 then 1 else amt)
--     <$> forAll Gen.contributions

--   let
--     sumAmts = sumAmounts cs

--   proportionalize cs
--     ===
--       ((fmap (fmap (fmap (fmap (\amt -> if sumAmts == 0 then 0 else (toRational amt / sumAmts) ))))) $ contributions cs)

-- prop_contributions_proportionalize_sumOne :: Property
-- prop_contributions_proportionalize_sumOne = property $ do
--   cause <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   ident <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   amt   <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   -- forall contributions for which there is at least 1 contribution > 0
--   cs    <-
--     contribute cause ident (if amt == 0 then 1 else amt)
--     <$> forAll Gen.contributions

--   let
--     sumProportions = getSum (foldMap (foldMap (foldMap (foldMap (Sum . toRational)))) (proportionalize cs))
--     sumAmts = sumAmounts cs

--   sumProportions === fromInteger 1

-- prop_contributions_denotation_contributionsBy :: Property
-- prop_contributions_denotation_contributionsBy = property $ do
--   ident <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   cs    <- forAll Gen.contributions

--   contributionsBy ident cs
--     === (foldMap (\(c, cs) -> foldMap (\(x, amt) -> if x == ident then [(c, amt)] else []) cs) $ contributions cs)

-- prop_contributions_denotation_contributionsFor :: Property
-- prop_contributions_denotation_contributionsFor = property $ do
--   cause <- forAll $ Gen.word8 (Range.linear 0 maxBound)
--   cs    <- forAll Gen.contributions

--   contributionsFor cause cs
--     === (foldMap (\(c, cs) -> if c == cause then cs else []) $ contributions cs)

-- prop_contributions_denotation_causeSumAmounts :: Property
-- prop_contributions_denotation_causeSumAmounts = property $ do
--   cs    <- forAll Gen.contributions

--   causeSumAmounts cs
--     === (fmap (fmap (getSum . foldMap (Sum . toRational . snd))) $ contributions cs)

-- -- prop_contributions_getRegistration :: Property
-- -- prop_contributions_getRegistration = property $ do
-- --   ident <- forAll $ Gen.int (Range.linear 0 maxBound)
-- --   r     <- forAll Gen.contributions

-- --   getRegistration ident r === (fmap snd $ find ((ident ==) . fst) $ contributions r)

-- -- prop_contributions_isRegistered :: Property
-- -- prop_contributions_isRegistered = property $ do
-- --   ident <- forAll $ Gen.int (Range.linear 0 maxBound)
-- --   r     <- forAll Gen.contributions

-- --   isRegistered ident r === maybe False (const True) (getRegistration ident r)

-- -- prop_contributions_isNotRegistered :: Property
-- -- prop_contributions_isNotRegistered = property $ do
-- --   ident <- forAll $ Gen.int (Range.linear 0 maxBound)
-- --   r     <- forAll Gen.contributions

-- --   isNotRegistered ident r === (not $ isRegistered ident r)
