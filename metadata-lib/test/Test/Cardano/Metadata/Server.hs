{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Test.Cardano.Metadata.Server
  ( tests
  , testsFns
  ) where

import           Prelude hiding (read)
import           Control.Monad.IO.Class (liftIO)
import           Servant.API
import           Data.Traversable
import           Data.Functor.Identity (Identity(Identity))
import           Data.Functor (void)
import           Data.Proxy (Proxy(Proxy))
import           Control.Monad (join)
import           Data.Text (Text)
import           Data.Word
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import           Text.Read (readEither)
import           Text.RawString.QQ
import           qualified Data.HashMap.Strict as HM
import           Hedgehog (Gen, MonadTest, annotate, forAll, property, tripping, (===), footnote, failure, evalIO, diff)
import qualified Hedgehog as H (Property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.Aeson (ToJSON, FromJSON)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit (Assertion, assertEqual, testCase, (@?=))
import Test.Tasty.Hspec
import qualified Network.Wai.Handler.Warp         as Warp
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Data.Map.Strict (Map)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M

import Test.SmallCheck.Series

import qualified Test.Generators as Gen
import Cardano.Metadata.Server
import Cardano.Metadata.Server.Types 
import Cardano.Metadata.Store.KeyValue.Map
import Cardano.Metadata.Store.KeyValue.Map as Simple
import Cardano.Metadata.Store.Types

withMetadataServerApp :: ReadFns -> (Warp.Port -> IO ()) -> IO ()
withMetadataServerApp readFns action =
  -- testWithApplication makes sure the action is executed after the server has
  -- started and is being properly shutdown.
  Warp.testWithApplication (pure $ webApp readFns) action

-- tests :: IO TestTree
-- tests = do
--   eg <- liftIO $ testSpec "eg" spec_eg
--   pure $
--     testGroup "Servant server tests"
--     [ eg
--     ]

-- spec_eg :: Spec
-- spec_eg = do
--   let
--     testData = M.fromList
--       [ ("3", Entry $ EntryF "3" (Identity $ Owner mempty mempty) (Identity $ Property "n" []) (Identity $ Property "d" []) (Identity $ PreImage "x" SHA256))
--       ]
        
--   around (withMetadataServerApp (readFnsSimple testData)) $ do
--     let getSubject :<|> getSubjectProperties :<|> getProperty :<|> getBatch = client (Proxy :: Proxy MetadataServerAPI)
--     baseUrl <- runIO $ parseBaseUrl "http://localhost"
--     manager <- runIO $ newManager defaultManagerSettings
--     let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

--     describe "GET /subject/{subject}" $ do
--       it "should return the subject" $ \port -> do
--         result <- runClientM (getSubject "3") (clientEnv port)
--         result `shouldBe` (Right $ Entry $ EntryF "3" (Identity $ Owner mempty mempty) (Identity $ Property "n" []) (Identity $ Property "d" []) (Identity $ PreImage "x" SHA256))
--     describe "GET /subject/{subject}/properties" $ do
--       it "should return the subject" $ \port -> do
--         result <- runClientM (getSubject "3") (clientEnv port)
--         result `shouldBe` (Right $ Entry $ EntryF "3" (Identity $ Owner mempty mempty) (Identity $ Property "n" []) (Identity $ Property "d" []) (Identity $ PreImage "x" SHA256))
--     describe "GET /subject/{subject}/property/{property}" $ do
--       it "should return the subject's property" $ \port -> do
--         result <- runClientM (getProperty "3" "owner") (clientEnv port)
--         result `shouldBe` (Right $ PartialEntry $ EntryF "3" (Just $ Owner mempty mempty) Nothing Nothing Nothing)
--     describe "GET /query" $ do
--       it "should return empty response if subject not found" $ \port -> do
--         result <- runClientM (getBatch $ BatchRequest ["3"] ["owner"]) (clientEnv port)
--         result `shouldBe` (Right $ BatchResponse [])
--       it "should return empty response if subject found but property not" $ \port -> do
--         result <- runClientM (getBatch $ BatchRequest ["3"] ["owner"]) (clientEnv port)
--         result `shouldBe` (Right $ BatchResponse [])
--       it "should ignore properties not found, returning properties that were found" $ \port -> do
--         result <- runClientM (getBatch $ BatchRequest ["3"] ["owner"]) (clientEnv port)
--         result `shouldBe` (Right $ BatchResponse [])
--       it "should return a batch response" $ \port -> do
--         result <- runClientM (getBatch $ BatchRequest ["3"] ["owner"]) (clientEnv port)
--         result `shouldBe` (Right $ BatchResponse [])

tests :: IO TestTree
tests = do
  (kvs :: KeyValue Word8 Word8) <- liftIO $ Simple.init mempty
  let
    f = StoreInterface (\k   -> Simple.read k kvs)
                       (\k v -> void $ Simple.write k v kvs)
                       (\k   -> void $ Simple.delete k kvs)
                       (\f k -> void $ Simple.update f k kvs)
                       (Simple.toList kvs)
                       (void $ Simple.empty kvs)
  pure $ testGroup "Data store implementations"
    [
      testGroup "Simple implementation" [testsFns f]
    ]

-- Presumes tests are done sequentially, and that the tests are the
-- only ones modifying the key-value store.
testsFns :: StoreInterface Word8 Word8 -> TestTree
testsFns f = do
  testGroup "Data store property tests"
    [ testProperty "write/denotation"  (prop_write_denotation f)
    , testProperty "write/last-wins"   (prop_write_last_wins f)
    , testProperty "delete/denotation" (prop_delete_denotation f)
    , testProperty "delete/cancels-write" (prop_delete_cancels_write f)
    , testProperty "delete/idempotent" (prop_delete_idempotent f)
    , testProperty "read/denotation"   (prop_read_denotation f)
    , testProperty "read/observation"  (prop_read_observation f)
    , testProperty "update/denotation" (prop_update_denotation f)
    ]

reset :: StoreInterface k v -> [(k, v)] -> IO ()
reset (StoreInterface _ write _ _ _ empty) kvs = void $ empty >> traverse (uncurry write) kvs

-- Write follows the semantics of a Map insert.
-- ∀k v. (write k v >> toList) = M.toList . M.insert k v . M.fromList <$> toList
prop_write_denotation :: StoreInterface Word8 Word8 -> H.Property
prop_write_denotation f@(StoreInterface _ write _ _ toList _) = property $ do
  k        <- forAll Gen.key
  v        <- forAll Gen.val
  kvs      <- forAll Gen.keyVals

  join $ evalIO $ do
    reset f kvs
    result   <- write k v >> toList
    reset f kvs
    expected <- M.toList . M.insert k v . M.fromList <$> toList

    pure $
      result === expected

-- Delete follows the semantics of a Map delete.
-- ∀k. delete k >> toList = M.toList . M.delete k . M.fromList <$> toList
prop_delete_denotation :: StoreInterface Word8 Word8 -> H.Property
prop_delete_denotation f@(StoreInterface _ _ delete _ toList _) = property $ do
  k   <- forAll Gen.key
  v   <- forAll Gen.val
  kvs <- forAll Gen.keyVals

  join $ evalIO $ do
    reset f kvs
    result <- delete k >> toList

    reset f kvs
    expected <- M.toList . M.delete k . M.fromList <$> toList

    pure $
      result === expected

-- Delete should cancel a write.
-- ∀k v. write k v >> delete k >> toList = delete k >> toList
prop_delete_cancels_write :: StoreInterface Word8 Word8 -> H.Property
prop_delete_cancels_write f@(StoreInterface _ write delete _ toList _) = property $ do
  k   <- forAll Gen.key
  v   <- forAll Gen.val
  kvs <- forAll Gen.keyVals

  join $ evalIO $ do
    reset f kvs
    result   <- write k v >> delete k >> toList

    reset f kvs
    expected <- delete k >> toList 

    pure $
      result === expected

-- Read should exhibit the following semantics:
-- ∀k v. not (member k) => read k = Nothing
-- ∀k v. member k       => read k = Just v
prop_read_denotation :: StoreInterface Word8 Word8 -> H.Property
prop_read_denotation f@(StoreInterface read write delete _ _ _) = property $ do
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

-- Read, as an observation, should be derivable from the algebra's
-- canonical observation (denotation) "toList".
-- ∀k. read k = M.lookup k . M.fromList <$> toList
prop_read_observation :: StoreInterface Word8 Word8 -> H.Property
prop_read_observation f@(StoreInterface read _ _ _ toList _) = property $ do
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
prop_write_last_wins :: StoreInterface Word8 Word8 -> H.Property
prop_write_last_wins f@(StoreInterface _ write _ _ toList _) = property $ do
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
      result === expected

-- Delete is an idempotent operation (doing it twice is the same as
-- doing it once).
-- ∀k. delete k >> delete k = delete k
prop_delete_idempotent :: StoreInterface Word8 Word8 -> H.Property
prop_delete_idempotent f@(StoreInterface _ write delete _ toList _) = property $ do
  k   <- forAll Gen.key
  v   <- forAll Gen.val
  kvs <- forAll Gen.keyVals

  join $ evalIO $ do
    reset f kvs
    result   <- delete k >> delete k >> toList

    reset f kvs
    expected <- delete k >> toList

    pure $ do
      result === expected

-- Update should follow the behaviour of delete or write, depending on
-- the result of the function given.
-- ∀k v kvs. update fv k kvs = read k kvs >>= (\case (Nothing -> pure kvs) (Just _  -> case mv of (Nothing -> delete k kvs) (Just v  -> write k v kvs)))
prop_update_denotation :: StoreInterface Word8 Word8 -> H.Property
prop_update_denotation f@(StoreInterface read write delete update toList _) = property $ do
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
      result === expected
