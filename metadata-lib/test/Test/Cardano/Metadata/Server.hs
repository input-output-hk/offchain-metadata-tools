{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Test.Cardano.Metadata.Server
  ( tests
  ) where

import           Prelude hiding (read)
import           Control.Monad.IO.Class (liftIO)
import           Servant.API
import           Data.Traversable
import           Data.String (fromString)
import           Data.Foldable
import           Data.Functor.Identity (Identity(Identity))
import           Data.Functor (void)
import           Data.Proxy (Proxy(Proxy))
import           Data.Monoid (First(First))
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
import           Test.Hspec.Wai
import           Test.Tasty.HUnit (Assertion, assertEqual, testCase, (@?=))
import Test.Tasty.Hspec
import qualified Network.Wai.Handler.Warp         as Warp
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Data.Map.Strict (Map)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Network.HTTP.Types 

import Test.Cardano.Metadata.Store
import Test.Cardano.Metadata.Generators (ComplexType)
import Cardano.Metadata.Server
import Cardano.Metadata.Server.Types 
import Cardano.Metadata.Store.Types
import Cardano.Metadata.Store.Simple (simpleStore)

tests :: IO TestTree
tests = do
  intf <- simpleStore mempty
  testSpec "Server tests" (spec_server intf)

withMetadataServerApp :: StoreInterface Subject Entry' -> (Warp.Port -> IO ()) -> IO ()
withMetadataServerApp intf action =
  -- testWithApplication makes sure the action is executed after the server has
  -- started and is being properly shutdown.
  Warp.testWithApplication (pure $ webApp intf) action

matchingJSON :: ToJSON a => a -> ResponseMatcher
matchingJSON = fromString . BLC.unpack . Aeson.encode

spec_server :: StoreInterface Subject Entry' -> Spec
spec_server intf@(StoreInterface { storeWrite = write }) = do
  let
    entry1 = Entry' "3" $ Entry $ EntryF (Identity $ Owner "me" mempty) (Identity $ GenericProperty "n" []) (Identity $ GenericProperty "d" []) (Identity $ PreImage "x" SHA256)
    entry2 = Entry' "4" $ Entry $ EntryF (Identity $ Owner "you" mempty) (Identity $ GenericProperty "aName" []) (Identity $ GenericProperty "aDescription" []) (Identity $ PreImage "y" Blake2b256)
    testData =
      [ (_eSubject entry1, entry1)
      , (_eSubject entry2, entry2)
      ]
        
  runIO $ traverse_ (uncurry write) testData

  with (pure $ webApp intf) $ do
    describe "GET /metadata/{subject}" $ do
      it "should return 404 if subject doesn't exist" $ 
        get "/metadata/bad" `shouldRespondWith` "Requested subject 'bad' not found" { matchStatus = 404 }
      it "should return the subject if it does exist" $ 
        get "/metadata/3" `shouldRespondWith` (matchingJSON entry1) { matchStatus = 200 }
    describe "GET /metadata/{subject}/properties" $ do
      it "should return 404 if subject doesn't exist" $ 
        get "/metadata/bad/properties" `shouldRespondWith` "Requested subject 'bad' not found" { matchStatus = 404 }
      it "should return the properties of the subject if it does exist" $ 
        get "/metadata/3/properties" `shouldRespondWith` (matchingJSON entry1) { matchStatus = 200 }
    describe "GET /metadata/{subject}/properties/{property}" $ do
      it "should return 404 if subject doesn't exist" $ 
        get "/metadata/bad/properties/owner" `shouldRespondWith` "Requested subject 'bad' not found" { matchStatus = 404 }
      it "should return 404 if property doesn't exist" $ 
        get "/metadata/3/properties/bad" `shouldRespondWith` "Requested subject '3' does not have the property 'bad'" { matchStatus = 404 }
      it "should return the property if it does exist" $ do
        get "/metadata/3/properties/owner" `shouldRespondWith` (matchingJSON $ (PartialEntry' "3" $ PartialEntry $ mempty { enOwner = First $ Just $ Owner "me" mempty })) { matchStatus = 200 }
        get "/metadata/3/properties/subject" `shouldRespondWith` (matchingJSON $ PartialEntry' "3" mempty) { matchStatus = 200 }
        get "/metadata/3/properties/name" `shouldRespondWith` (matchingJSON $ (PartialEntry' "3" $ PartialEntry $ mempty { enName = First $ Just $ GenericProperty "n" mempty })) { matchStatus = 200 }
        get "/metadata/3/properties/description" `shouldRespondWith` (matchingJSON $ (PartialEntry' "3" $ PartialEntry $ mempty { enDescription = First $ Just $ GenericProperty "d" mempty })) { matchStatus = 200 }
        get "/metadata/3/properties/preImage" `shouldRespondWith` (matchingJSON $ (PartialEntry' "3" $ PartialEntry $ mempty { enPreImage = First $ Just $ PreImage "x" SHA256 })) { matchStatus = 200 }
    describe "GET /metadata/query" $ do
      it "should return empty response if subject not found" $ do
        request methodPost "/metadata/query" [(hContentType, "application/json")] (Aeson.encode $ BatchRequest ["bad"] [])
          `shouldRespondWith` (matchingJSON $ BatchResponse [])
        request methodPost "/metadata/query" [(hContentType, "application/json")] (Aeson.encode $ BatchRequest ["bad"] ["owner"])
          `shouldRespondWith` (matchingJSON $ BatchResponse [])
      it "should ignore subjects not found, returning subjects that were found" $ 
        request methodPost "/metadata/query" [(hContentType, "application/json")] (Aeson.encode $ BatchRequest ["3", "bad"] [])
          `shouldRespondWith` (matchingJSON $ BatchResponse [PartialEntry' "3" mempty])
      it "should return partial response if subject found but property not" $
        request methodPost "/metadata/query" [(hContentType, "application/json")] (Aeson.encode $ BatchRequest ["3"] ["bad"])
          `shouldRespondWith` (matchingJSON $ BatchResponse [PartialEntry' "3" mempty])
      it "should ignore properties not found, returning properties that were found" $ 
        request methodPost "/metadata/query" [(hContentType, "application/json")] (Aeson.encode $ BatchRequest ["3"] ["owner", "bad"])
          `shouldRespondWith` (matchingJSON $ BatchResponse [PartialEntry' "3" $ PartialEntry $ mempty { enOwner = First $ Just $ Owner "me" mempty }])
      it "should return a batch response" $ do
        request methodPost "/metadata/query" [(hContentType, "application/json")] (Aeson.encode $ BatchRequest ["3"] ["owner", "subject", "preImage"])
          `shouldRespondWith` (matchingJSON $ BatchResponse [PartialEntry' "3" $ PartialEntry $ mempty { enOwner = First $ Just $ Owner "me" mempty, enPreImage = First $ Just $ PreImage "x" SHA256 }])
        request methodPost "/metadata/query" [(hContentType, "application/json")] (Aeson.encode $ BatchRequest ["3","4"] ["owner", "subject", "preImage"])
          `shouldRespondWith`
            (matchingJSON $ BatchResponse
              [ PartialEntry' "3" $ PartialEntry $ mempty { enOwner = First $ Just $ Owner "me" mempty, enPreImage = First $ Just $ PreImage "x" SHA256 }
              , PartialEntry' "4" $ PartialEntry $ mempty { enOwner = First $ Just $ Owner "you" mempty, enPreImage = First $ Just $ PreImage "y" Blake2b256 }
              ])
