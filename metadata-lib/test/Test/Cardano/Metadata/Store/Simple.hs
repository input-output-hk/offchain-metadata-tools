{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Test.Cardano.Metadata.Store.Simple
  ( tests
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
import           Test.Tasty (TestTree, testGroup, withResource)
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit (Assertion, assertEqual, testCase, (@?=))
import Test.Tasty.Hspec
import qualified Network.Wai.Handler.Warp         as Warp
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Data.Map.Strict (Map)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M

import Test.Cardano.Metadata.Store
import Test.Cardano.Metadata.Generators (ComplexType)
import Cardano.Metadata.Server
import Cardano.Metadata.Server.Types 
import Cardano.Metadata.Store.Types
import Cardano.Metadata.Store.Simple (simpleStore)

tests :: TestTree
tests =
  withResource
    (do
        simpleIntf1 <- simpleStore mempty
        simpleIntf2 <- simpleStore mempty
        pure (simpleIntf1, simpleIntf2)
    )
    (const $ pure ())
    testsWithStoreInterfaces

testsWithStoreInterfaces
  :: IO ( StoreInterface Word8 Word8
        , StoreInterface Text ComplexType
        )
  -> TestTree
testsWithStoreInterfaces intfs = do
  testGroup "Data store implementations"
    [ testGroup "Simple implementation key-value"     [testKeyValueImplementation (fst <$> intfs)]
    , testGroup "Simple implementation complex types" [testKeyValueComplexTypeImplementation (snd <$> intfs)]
    ]
