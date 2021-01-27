{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Cardano.Metadata.Server
  ( tests
  ) where

import           Data.List (delete, find, sort)
import           Control.Monad.IO.Class (liftIO)
import           Servant.API
import           Data.Traversable
import           Data.Functor.Identity (Identity(Identity))
import           Data.Proxy (Proxy(Proxy))
import           Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import           Text.Read (readEither)
import           Text.RawString.QQ
import           qualified Data.HashMap.Strict as HM
import           Hedgehog (Gen, MonadTest, annotate, forAll, property, tripping, (===), footnote, failure)
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

withMetadataServerApp :: ReadFns -> (Warp.Port -> IO ()) -> IO ()
withMetadataServerApp readFns action =
  -- testWithApplication makes sure the action is executed after the server has
  -- started and is being properly shutdown.
  Warp.testWithApplication (pure $ webApp readFns) action

tests :: IO TestTree
tests = do
  eg <- liftIO $ testSpec "eg" spec_eg
  pure $
    testGroup "Servant server tests"
    [ eg
    ]

spec_eg :: Spec
spec_eg = do
  let
    testData = M.fromList
      [ ("3", Entry $ EntryF "3" (Identity $ Owner mempty mempty) (Identity $ Property "n" []) (Identity $ Property "d" []) (Identity $ PreImage "x" SHA256))
      ]
        
  around (withMetadataServerApp (readFnsSimple testData)) $ do
    let getSubject :<|> getSubjectProperties :<|> getProperty :<|> getBatch = client (Proxy :: Proxy MetadataServerAPI)
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

    describe "GET /subject/{subject}" $ do
      it "should return the subject" $ \port -> do
        result <- runClientM (getSubject "3") (clientEnv port)
        result `shouldBe` (Right $ Entry $ EntryF "3" (Identity $ Owner mempty mempty) (Identity $ Property "n" []) (Identity $ Property "d" []) (Identity $ PreImage "x" SHA256))
    describe "GET /subject/{subject}/properties" $ do
      it "should return the subject" $ \port -> do
        result <- runClientM (getSubject "3") (clientEnv port)
        result `shouldBe` (Right $ Entry $ EntryF "3" (Identity $ Owner mempty mempty) (Identity $ Property "n" []) (Identity $ Property "d" []) (Identity $ PreImage "x" SHA256))
    describe "GET /subject/{subject}/property/{property}" $ do
      it "should return the subject's property" $ \port -> do
        result <- runClientM (getProperty "3" "owner") (clientEnv port)
        result `shouldBe` (Right $ PartialEntry $ EntryF "3" (Just $ Owner mempty mempty) Nothing Nothing Nothing)
    describe "GET /query" $ do
      it "should return empty response if subject not found" $ \port -> do
        result <- runClientM (getBatch $ BatchRequest ["3"] ["owner"]) (clientEnv port)
        result `shouldBe` (Right $ BatchResponse [])
      it "should return empty response if subject found but property not" $ \port -> do
        result <- runClientM (getBatch $ BatchRequest ["3"] ["owner"]) (clientEnv port)
        result `shouldBe` (Right $ BatchResponse [])
      it "should ignore properties not found, returning properties that were found" $ \port -> do
        result <- runClientM (getBatch $ BatchRequest ["3"] ["owner"]) (clientEnv port)
        result `shouldBe` (Right $ BatchResponse [])
      it "should return a batch response" $ \port -> do
        result <- runClientM (getBatch $ BatchRequest ["3"] ["owner"]) (clientEnv port)
        result `shouldBe` (Right $ BatchResponse [])

readFnsSimple :: Map Subject Entry -> ReadFns
readFnsSimple dat = ReadFns
  (pure . getEntryForSubject)
  (\subj -> pure . getPartialEntryForProperty subj)
  (pure . getBatch)

  where
    getEntryForSubject :: Subject -> Either ReadError Entry
    getEntryForSubject subj = case M.lookup subj dat of
      Nothing -> Left $ NoSubject subj
      Just e  -> Right e

    getPartialEntryForProperty :: Subject -> Text -> Either ReadError PartialEntry
    getPartialEntryForProperty subj prop = do
      entry <- getEntryForSubject subj
      getProperty subj prop entry

    getProperty :: Subject -> Text -> Entry -> Either ReadError PartialEntry
    getProperty subj prop entry =
      case Aeson.toJSON entry of
        (Aeson.Object obj) -> case HM.lookup prop obj of
          Nothing -> Left $ NoProperty subj prop
          Just p  -> case Aeson.fromJSON (Aeson.Object $ HM.fromList [("subject", Aeson.String subj), (prop, p)]) of
            Aeson.Error str -> error $ "JSON parsing error: " <> str
            Aeson.Success x -> Right x
        otherwise          -> error "Entry isn't a JSON Object but should be."

    getBatch :: BatchRequest -> BatchResponse
    getBatch (BatchRequest subjs props) = BatchResponse $
      either (const []) mconcat $
        forM subjs $ \subj ->
          forM props $ \prop ->
            case getPartialEntryForProperty subj prop of
              _ -> 
