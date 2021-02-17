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
import           Data.Maybe (fromJust)
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
import Network.URI (parseURI)

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
    entry1 = Entry' "3" $ Entry (GenericProperty "n" []) (GenericProperty "d" []) (Just $ Owner "me" mempty) (Just $ GenericProperty "ABCD" []) (Just $ GenericProperty (AssetURL $ fromJust $ parseURI "https://google.com") []) (Just $ GenericProperty (AssetLogo mempty) []) (Just $ GenericProperty (AssetUnit "dave" 10) [])
    entry2 = Entry' "4" $ Entry (GenericProperty "aName" []) (GenericProperty "aDescription" []) (Just $ Owner "you" mempty) Nothing Nothing Nothing Nothing
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
        get "/metadata/3/properties/acronym" `shouldRespondWith` (matchingJSON $ (PartialEntry' "3" $ PartialEntry $ mempty { enAcronym = First $ Just $ GenericProperty "ABCD" [] })) { matchStatus = 200 }
        get "/metadata/3/properties/unit" `shouldRespondWith` (matchingJSON $ (PartialEntry' "3" $ PartialEntry $ mempty { enUnit = First $ Just $ GenericProperty (AssetUnit "dave" 10) [] })) { matchStatus = 200 }
        get "/metadata/3/properties/url" `shouldRespondWith` (matchingJSON $ (PartialEntry' "3" $ PartialEntry $ mempty { enURL = First $ Just $ GenericProperty (AssetURL $ fromJust $ parseURI "https://google.com") [] })) { matchStatus = 200 }
        get "/metadata/3/properties/logo" `shouldRespondWith` (matchingJSON $ (PartialEntry' "3" $ PartialEntry $ mempty { enLogo = First $ Just $ GenericProperty (AssetLogo mempty) [] })) { matchStatus = 200 }
    describe "GET /metadata/query" $ do
      it "should return empty response if subject not found" $ do
        request methodPost "/metadata/query" [(hContentType, "application/json")] (Aeson.encode $ BatchRequest ["bad"] (Just []))
          `shouldRespondWith` (matchingJSON $ BatchResponse [])
        request methodPost "/metadata/query" [(hContentType, "application/json")] (Aeson.encode $ BatchRequest ["bad"] (Just ["owner"]))
          `shouldRespondWith` (matchingJSON $ BatchResponse [])
      it "should ignore subjects not found, returning subjects that were found" $ 
        request methodPost "/metadata/query" [(hContentType, "application/json")] (Aeson.encode $ BatchRequest ["3", "bad"] (Just []))
          `shouldRespondWith` (matchingJSON $ BatchResponse [PartialEntry' "3" mempty])
      it "should return partial response if subject found but property not" $
        request methodPost "/metadata/query" [(hContentType, "application/json")] (Aeson.encode $ BatchRequest ["3"] (Just ["bad"]))
          `shouldRespondWith` (matchingJSON $ BatchResponse [PartialEntry' "3" mempty])
      it "should ignore properties not found, returning properties that were found" $ 
        request methodPost "/metadata/query" [(hContentType, "application/json")] (Aeson.encode $ BatchRequest ["3"] (Just ["owner", "bad"]))
          `shouldRespondWith` (matchingJSON $ BatchResponse [PartialEntry' "3" $ PartialEntry $ mempty { enOwner = First $ Just $ Owner "me" mempty }])
      it "should return a batch response" $ do
        request methodPost "/metadata/query" [(hContentType, "application/json")] (Aeson.encode $ BatchRequest ["3"] (Just ["owner", "subject"]))
          `shouldRespondWith` (matchingJSON $ BatchResponse [PartialEntry' "3" $ PartialEntry $ mempty { enOwner = First $ Just $ Owner "me" mempty }])
        request methodPost "/metadata/query" [(hContentType, "application/json")] (Aeson.encode $ BatchRequest ["3","4"] (Just ["owner", "subject"]))
          `shouldRespondWith`
            (matchingJSON $ BatchResponse
              [ PartialEntry' "3" $ PartialEntry $ mempty { enOwner = First $ Just $ Owner "me" mempty }
              , PartialEntry' "4" $ PartialEntry $ mempty { enOwner = First $ Just $ Owner "you" mempty }
              ])
      it "should return all properties if key 'properties' not present in JSON request" $
        request methodPost "/metadata/query" [(hContentType, "application/json")] (Aeson.encode $ BatchRequest ["3"] Nothing) 
          `shouldRespondWith` (matchingJSON $ BatchResponse [PartialEntry' "3" $ PartialEntry $ EntryF { enName = First $ Just $ GenericProperty "n" mempty, enDescription = First $ Just $ GenericProperty "d" mempty, enOwner = First $ Just $ Owner "me" mempty, enAcronym = First $ Just $ GenericProperty "ABCD" [], enURL = First $ Just $ GenericProperty (AssetURL $ fromJust $ parseURI "https://google.com") [], enLogo = First $ Just $ GenericProperty (AssetLogo mempty) [], enUnit = First $ Just $ GenericProperty (AssetUnit "dave" 10) [] }])
