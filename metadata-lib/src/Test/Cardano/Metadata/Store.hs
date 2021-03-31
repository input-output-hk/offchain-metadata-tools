{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Metadata.Store
  ( testKeyValueImplementation
  , testKeyValueComplexTypeImplementation
  ) where

import           Control.Monad                    (join)
import           Data.Functor                     (void)
import           Data.List                        (sort)
import qualified Data.Map.Strict                  as M
import           Data.Maybe                       (catMaybes)
import           Data.Word
import           Hedgehog                         (evalIO, forAll, property,
                                                   (===))
import qualified Hedgehog                         as H (Property)
import qualified Hedgehog.Gen                     as Gen
import qualified Hedgehog.Range                   as Range
import           Prelude                          hiding (read)
import           Test.Tasty
import           Test.Tasty.Hedgehog

import           Cardano.Metadata.Store.Types
import qualified Test.Cardano.Metadata.Generators as Gen

-- | Tests the ability of the given store interface to act as a
-- key-value store.
--
-- Presumes tests are done sequentially, and that the tests are the
-- only ones modifying the key-value store.
testKeyValueImplementation :: IO (StoreInterface Word8 Word8) -> TestTree
testKeyValueImplementation f = do
  testGroup "Data store property tests"
    [ testProperty "write/denotation"  (prop_write_denotation f)
    , testProperty "write/last-wins"   (prop_write_last_wins f)
    , testProperty "delete/denotation" (prop_delete_denotation f)
    , testProperty "delete/cancels-write" (prop_delete_cancels_write f)
    , testProperty "delete/idempotent" (prop_delete_idempotent f)
    , testProperty "read/denotation"   (prop_read_denotation f)
    , testProperty "read/observation"  (prop_read_observation f)
    , testProperty "readBatch/denotation"   (prop_readBatch_denotation f)
    , testProperty "update/denotation" (prop_update_denotation f)
    ]

-- | Test the ability of the given store interface to read and write
-- complex types.
testKeyValueComplexTypeImplementation :: IO (StoreInterface Gen.ComplexKey Gen.ComplexType) -> TestTree
testKeyValueComplexTypeImplementation f = do
  testGroup "Data store complex type tests"
    [ testProperty "complex/write/read" (prop_complex_write_read f)
    , testProperty "complex/delete" (prop_complex_delete f)
    , testProperty "complex/update" (prop_complex_update f)
    ]

prop_complex_write_read :: IO (StoreInterface Gen.ComplexKey Gen.ComplexType) -> H.Property
prop_complex_write_read getIntf = property $ do
  (f@(StoreInterface { storeRead = read, storeWrite = write })) <- evalIO getIntf
  k   <- forAll Gen.complexKey
  v   <- forAll Gen.complexType
  kvs <- forAll Gen.complexKeyVals

  join $ evalIO $ do
    reset f kvs
    result <- write k v >> read k

    pure $
      result === Just v

prop_complex_delete :: IO (StoreInterface Gen.ComplexKey Gen.ComplexType) -> H.Property
prop_complex_delete getIntf = property $ do
  (f@(StoreInterface { storeRead = read, storeWrite = write, storeDelete = delete })) <- evalIO getIntf
  k   <- forAll Gen.complexKey
  v   <- forAll Gen.complexType
  kvs <- forAll Gen.complexKeyVals

  join $ evalIO $ do
    reset f kvs
    write k v >> delete k

    result <- read k

    pure $
      result === Nothing

prop_complex_update :: IO (StoreInterface Gen.ComplexKey Gen.ComplexType) -> H.Property
prop_complex_update getIntf = property $ do
  (f@(StoreInterface { storeRead = read, storeWrite = write, storeUpdate = update })) <- evalIO getIntf
  k   <- forAll Gen.complexKey
  v   <- forAll Gen.complexType
  kvs <- forAll Gen.complexKeyVals

  join $ evalIO $ do
    let fun (Gen.ComplexType xs m) = Just $ Gen.ComplexType (3 : xs) m

    reset f kvs
    write k v >> update fun k

    result <- read k

    pure $
      result === fun v

reset :: StoreInterface k v -> [(k, v)] -> IO ()
reset (StoreInterface { storeWrite = write, storeEmpty = empty }) kvs = void $ empty >> traverse (uncurry write) kvs

-- Write follows the semantics of a Map insert.
-- ∀k v. (write k v >> toList) = M.toList . M.insert k v . M.fromList <$> toList
prop_write_denotation :: IO (StoreInterface Word8 Word8) -> H.Property
prop_write_denotation getIntf = property $ do
  (f@(StoreInterface { storeWrite = write, storeToList = toList })) <- evalIO getIntf
  k        <- forAll Gen.key
  v        <- forAll Gen.val
  kvs      <- forAll Gen.keyVals

  join $ evalIO $ do
    reset f kvs
    result   <- write k v >> toList
    reset f kvs
    expected <- M.toList . M.insert k v . M.fromList <$> toList

    pure $
      sort result === sort expected

-- Delete follows the semantics of a Map delete.
-- ∀k. delete k >> toList = M.toList . M.delete k . M.fromList <$> toList
prop_delete_denotation :: IO (StoreInterface Word8 Word8) -> H.Property
prop_delete_denotation getIntf = property $ do
  (f@(StoreInterface { storeDelete = delete, storeToList = toList })) <- evalIO getIntf
  k   <- forAll Gen.key
  kvs <- forAll Gen.keyVals

  join $ evalIO $ do
    reset f kvs
    result <- delete k >> toList

    reset f kvs
    expected <- M.toList . M.delete k . M.fromList <$> toList

    pure $
      sort result === sort expected

-- Delete should cancel a write.
-- ∀k v. write k v >> delete k >> toList = delete k >> toList
prop_delete_cancels_write :: IO (StoreInterface Word8 Word8) -> H.Property
prop_delete_cancels_write getIntf = property $ do
  (f@(StoreInterface { storeWrite = write, storeDelete = delete, storeToList = toList })) <- evalIO getIntf
  k   <- forAll Gen.key
  v   <- forAll Gen.val
  kvs <- forAll Gen.keyVals

  join $ evalIO $ do
    reset f kvs
    result   <- write k v >> delete k >> toList

    reset f kvs
    expected <- delete k >> toList

    pure $
      sort result === sort expected

-- Read should exhibit the following semantics:
-- ∀k v. not (member k) => read k = Nothing
-- ∀k v. member k       => read k = Just v
prop_read_denotation :: IO (StoreInterface Word8 Word8) -> H.Property
prop_read_denotation getIntf = property $ do
  (f@(StoreInterface { storeRead = read, storeWrite = write, storeDelete = delete })) <- evalIO getIntf
  k   <- forAll Gen.key
  v   <- forAll Gen.val
  kvs <- forAll Gen.keyVals

  join $ evalIO $ do
    reset f kvs
    -- Given a key-value store without the key
    delete k
    -- Reading the key should always return Nothing
    read1 <- read k

    reset f kvs
    -- Given a key-value store with the key
    write k v
    -- Reading the key should always return Just v
    read2 <- read k

    pure $ do
      read1 === Nothing
      read2 === Just v

prop_readBatch_denotation :: IO (StoreInterface Word8 Word8) -> H.Property
prop_readBatch_denotation getIntf = property $ do
  (f@(StoreInterface { storeRead = read, storeReadBatch = readBatch })) <- evalIO getIntf
  ks  <- forAll $ Gen.list (Range.linear 0 20) Gen.key
  kvs <- forAll Gen.keyVals

  join $ evalIO $ do
    reset f kvs
    result <- readBatch ks

    reset f kvs
    expected <- traverse read ks >>= (pure . catMaybes)

    pure $ do
      result === expected

-- Read, as an observation, should be derivable from the algebra's
-- canonical observation (denotation) "toList".
-- ∀k. read k = M.lookup k . M.fromList <$> toList
prop_read_observation :: IO (StoreInterface Word8 Word8) -> H.Property
prop_read_observation getIntf = property $ do
  (f@(StoreInterface { storeRead = read, storeToList = toList })) <- evalIO getIntf
  k <- forAll Gen.key
  kvs <- forAll Gen.keyVals

  join $ evalIO $ do
    reset f kvs

    result   <- read k
    expected <- M.lookup k . M.fromList <$> toList

    pure $ do
      result === expected

-- The last value written to the key is the value of that key, also
-- covers idempotence.
-- ∀k v1 v2 . write k v1 >> write k v2 = write k v2
prop_write_last_wins :: IO (StoreInterface Word8 Word8) -> H.Property
prop_write_last_wins getIntf = property $ do
  (f@(StoreInterface { storeWrite = write, storeToList = toList })) <- evalIO getIntf

  k   <- forAll Gen.key
  v1  <- forAll Gen.val
  v2  <- forAll Gen.val
  kvs <- forAll Gen.keyVals

  join $ evalIO $ do
    reset f kvs
    result   <- write k v1 >> write k v2 >> toList

    reset f kvs
    expected <- write k v2 >> toList

    pure $ do
      sort result === sort expected

-- Delete is an idempotent operation (doing it twice is the same as
-- doing it once).
-- ∀k. delete k >> delete k = delete k
prop_delete_idempotent :: IO (StoreInterface Word8 Word8) -> H.Property
prop_delete_idempotent getIntf = property $ do
  (f@(StoreInterface { storeDelete = delete, storeToList = toList })) <- evalIO getIntf

  k   <- forAll Gen.key
  kvs <- forAll Gen.keyVals

  join $ evalIO $ do
    reset f kvs
    result   <- delete k >> delete k >> toList

    reset f kvs
    expected <- delete k >> toList

    pure $ do
      sort result === sort expected

-- Update should follow the behaviour of delete or write, depending on
-- the result of the function given.
-- ∀k v kvs. update fv k kvs = read k kvs >>= (\case (Nothing -> pure kvs) (Just _  -> case mv of (Nothing -> delete k kvs) (Just v  -> write k v kvs)))
prop_update_denotation :: IO (StoreInterface Word8 Word8) -> H.Property
prop_update_denotation getIntf = property $ do
  (f@(StoreInterface { storeRead = read, storeWrite = write, storeDelete = delete, storeUpdate = update, storeToList = toList })) <- evalIO getIntf

  k <- forAll Gen.key
  mv <- forAll $ Gen.maybe Gen.val
  let fun = const mv
  kvs <- forAll Gen.keyVals

  join $ evalIO $ do
    reset f kvs
    result <- update fun k >> toList

    reset f kvs
    expected <-
      read k
        >>= (\case
          Nothing -> pure ()
          Just _  -> case mv of
            Nothing -> delete k
            Just v  -> write k v
          )
        >> toList

    pure $ do
      sort result === sort expected
