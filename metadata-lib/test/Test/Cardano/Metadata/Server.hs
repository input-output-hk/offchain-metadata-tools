{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Test.Cardano.Metadata.Server
  ( tests
  , testsFns
  ) where

import           Prelude hiding (read)
import           Control.Monad.IO.Class (liftIO)
import           Servant.API
import           Data.Traversable
import           Data.Functor.Identity (Identity(Identity))
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

import qualified Test.Generators as Gen

import Cardano.Metadata.Server
import Cardano.Metadata.Server.Types 
import Cardano.Metadata.Store.KeyValue.Map
import Cardano.Metadata.Store.KeyValue.Map as Simple

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
  kvs <- liftIO $ Simple.init mempty
  pure $ testGroup "Data store implementations"
    [
      testGroup "Simple implementation" [testsFns kvs]
    ]

-- Presumes tests are done sequentially, and that the tests are the
-- only ones modifying the key-value store.
testsFns :: KeyValue Word8 Word8 -> TestTree
testsFns kvs = do
  testGroup "Data store property tests"
    [ testProperty "write/denotation"  (prop_write_denotation kvs)
    , testProperty "write/last-wins"   (prop_write_last_wins kvs)
    , testProperty "delete/denotation" (prop_delete_denotation kvs)
    , testProperty "delete/idempotent" (prop_delete_idempotent kvs)
    , testProperty "read/denotation"   (prop_read_denotation kvs)
    , testProperty "read/observation"  (prop_read_observation kvs)
    , testProperty "update/denotation" (prop_update_denotation kvs)
    ]
    
-- Write follows the semantics of a Map insert.
-- ∀k v kvs. (write k v kvs >>= toList) = M.toList . M.insert k v . M.fromList <$> toList kvs
prop_write_denotation :: KeyValue Word8 Word8 -> H.Property
prop_write_denotation kvs = property $ do
  k   <- forAll Gen.key
  v   <- forAll Gen.val

  join $ evalIO $ do
    result   <- write k v kvs >>= toList
    expected <- M.toList . M.insert k v . M.fromList <$> toList kvs

    pure $
      result === expected

-- Delete should cancel a write.
-- ∀k v kvs. write k v kvs >>= delete k >>= toList = toList kvs
prop_delete_denotation :: KeyValue Word8 Word8 -> H.Property
prop_delete_denotation kvs = property $ do
  k <- forAll Gen.key
  v <- forAll Gen.val

  join $ evalIO $ do
    result   <- write k v kvs >>= delete k >>= toList
    expected <- toList kvs

    pure $
      result === expected

-- Read should exhibit the following semantics:
-- ∀k v kvs. not (member k kvs) => read k kvs = Nothing
-- ∀k v kvs. member k kvs       => read k kvs = Just v
prop_read_denotation :: KeyValue Word8 Word8 -> H.Property
prop_read_denotation kvs = property $ do
  k <- forAll Gen.key
  v <- forAll Gen.val

  join $ evalIO $ do
    -- Given a key-value store without the key
    kvs1 <- delete k kvs
    -- Reading the key should always return Nothing
    read1 <- read k kvs1

    -- Given a key-value store with the key
    kvs2 <- write k v kvs
    -- Reading the key should always return Just v
    read2 <- read k kvs2

    pure $ do
      read1 === Nothing
      read2 === Just v

-- Read, as an observation, should be derivable from the algebra's
-- canonical observation (denotation) "toList".
-- ∀k v kvs. read k kvs = M.lookup k . M.fromList <$> toList kvs
prop_read_observation :: KeyValue Word8 Word8 -> H.Property
prop_read_observation kvs = property $ do
  k <- forAll Gen.key

  join $ evalIO $ do
    result   <- read k kvs
    expected <- M.lookup k . M.fromList <$> toList kvs

    pure $ do
      result === expected
    
-- The last value written to the key is the value of that key, also
-- covers idempotence.
-- ∀k v1 v2 kvs. write k v1 kvs >>= write k v2 = write k v2 kvs
prop_write_last_wins :: KeyValue Word8 Word8 -> H.Property
prop_write_last_wins kvs = property $ do
  k  <- forAll Gen.key
  v1 <- forAll Gen.val
  v2 <- forAll Gen.val

  join $ evalIO $ do
    result   <- write k v1 kvs >>= write k v2 >>= toList
    expected <- write k v2 kvs >>= toList

    pure $ do
      result === expected

-- Delete is an idempotent operation (doing it twice is the same as
-- doing it once).
-- ∀k kvs. delete k kvs >>= delete k = delete k kvs
prop_delete_idempotent :: KeyValue Word8 Word8 -> H.Property
prop_delete_idempotent kvs = property $ do
  k <- forAll Gen.key
  v <- forAll Gen.val

  join $ evalIO $ do
    kvs' <- write k v kvs

    result   <- delete k kvs' >>= delete k >>= toList
    expected <- delete k kvs' >>= toList

    pure $ do
      result === expected

-- Update should follow the behaviour of delete or write, depending on
-- the result of the function given.
-- ∀k v kvs. update fv k kvs = read k kvs >>= (\case (Nothing -> pure kvs) (Just _  -> case mv of (Nothing -> delete k kvs) (Just v  -> write k v kvs)))
prop_update_denotation :: KeyValue Word8 Word8 -> H.Property
prop_update_denotation kvs = property $ do
  k <- forAll Gen.key
  mv <- forAll $ Gen.maybe Gen.val
  let f = const mv

  join $ evalIO $ do
    result <- update f k kvs >>= toList
    expected <-
      read k kvs
        >>= (\case
          Nothing -> pure kvs
          Just _  -> case mv of
            Nothing -> delete k kvs
            Just v  -> write k v kvs
          )
        >>= toList

    pure $ do
      result === expected
