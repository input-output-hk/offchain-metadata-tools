{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Metadata.Store.Simple
  ( tests
  ) where

import           Control.Monad                    (join)
import           Control.Monad.IO.Class           (liftIO)
import           Data.Aeson                       (FromJSON, ToJSON)
import qualified Data.Aeson                       as Aeson
import qualified Data.Aeson                       as Aeson
import qualified Data.Aeson.Encode.Pretty         as Aeson
import qualified Data.ByteString.Lazy.Char8       as BLC
import           Data.Functor                     (void)
import           Data.Functor.Identity            (Identity (Identity))
import qualified Data.HashMap.Strict              as HM
import qualified Data.HashMap.Strict              as HM
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as M
import           Data.Proxy                       (Proxy (Proxy))
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Traversable
import           Data.Word
import           Hedgehog                         (Gen, MonadTest, annotate,
                                                   diff, evalIO, failure,
                                                   footnote, forAll, property,
                                                   tripping, (===))
import qualified Hedgehog                         as H (Property)
import qualified Hedgehog.Gen                     as Gen
import qualified Hedgehog.Range                   as Range
import           Network.HTTP.Client              (defaultManagerSettings,
                                                   newManager)
import qualified Network.Wai.Handler.Warp         as Warp
import           Prelude                          hiding (read)
import           Servant.API
import           Servant.Client
import           Test.Tasty                       (TestTree, testGroup,
                                                   withResource)
import           Test.Tasty.Hedgehog
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit                 (Assertion, assertEqual,
                                                   testCase, (@?=))
import           Text.RawString.QQ
import           Text.Read                        (readEither)

import           Cardano.Metadata.Server
import           Cardano.Metadata.Server.Types
import           Cardano.Metadata.Store.Simple    (simpleStore)
import           Cardano.Metadata.Store.Types
import           Test.Cardano.Metadata.Generators (ComplexType)
import           Test.Cardano.Metadata.Store

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
