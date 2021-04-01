{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Metadata.Store.Simple
  ( tests
  ) where

import Data.Text
    ( Text )
import Data.Word
import Prelude hiding
    ( read )
import Test.Tasty
    ( TestTree, testGroup, withResource )

import Cardano.Metadata.Store.Simple
    ( simpleStore )
import Cardano.Metadata.Store.Types
import Test.Cardano.Metadata.Generators
    ( ComplexType )
import Test.Cardano.Metadata.Store

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
