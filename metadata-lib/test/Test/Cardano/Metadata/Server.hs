{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Metadata.Server
  ( tests
  ) where

import           Control.Monad                    (join)
import           Control.Monad.IO.Class           (liftIO)
import           Data.Aeson                       (FromJSON, ToJSON)
import qualified Data.Aeson                       as Aeson
import qualified Data.Aeson                       as Aeson
import qualified Data.Aeson.Encode.Pretty         as Aeson
import qualified Data.ByteString.Char8            as BC
import qualified Data.ByteString.Lazy.Char8       as BLC
import           Data.Foldable
import           Data.Functor                     (void)
import           Data.Functor.Identity            (Identity (Identity))
import qualified Data.HashMap.Strict              as HM
import qualified Data.HashMap.Strict              as HM
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as M
import           Data.Maybe                       (fromJust)
import           Data.Monoid                      (First (First))
import           Data.Proxy                       (Proxy (Proxy))
import           Data.String                      (fromString)
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
import           Network.HTTP.Types
import           Network.URI                      (parseURI)
import qualified Network.Wai.Handler.Warp         as Warp
import           Prelude                          hiding (read)
import           Servant.API
import           Servant.Client
import           Test.Hspec.Wai
import           Test.Tasty                       (TestTree, testGroup,
                                                   withResource)
import           Test.Tasty.Hedgehog
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit                 (Assertion, assertEqual,
                                                   testCase, (@?=))
import           Text.RawString.QQ
import           Text.Read                        (readEither)

import           Cardano.Metadata.Server
import           Cardano.Metadata.Server.Types    (BatchRequest (BatchRequest),
                                                   BatchResponse (BatchResponse))
import           Cardano.Metadata.Store.Simple    (simpleStore)
import           Cardano.Metadata.Store.Types
import           Cardano.Metadata.Types.Common    (Property (Property),
                                                   Subject (Subject), unSubject)
import qualified Cardano.Metadata.Types.Weakly    as Weakly
import           Test.Cardano.Metadata.Generators (ComplexType)
import           Test.Cardano.Metadata.Store

tests :: IO TestTree
tests = do
  intf <- simpleStore mempty
  testSpec "Server tests" (spec_server intf)

spec_server
  :: StoreInterface Subject Weakly.Metadata
  -> Spec
spec_server intf@(StoreInterface { storeWrite = write }) = do
  let
    subject1    = Subject "3"
    subject1Str = BC.pack . T.unpack . unSubject $ subject1
    subject2    = Subject "4"
    subject2Str = BC.pack . T.unpack . unSubject $ subject2
    owner       = Property (Aeson.String "me") []
    odd         = Property (Aeson.String "odd") []
    random      = Property (Aeson.String "random") []
    entry1 = Weakly.Metadata subject1 (HM.fromList [("owner", owner), ("odd", odd)])
    entry2 = Weakly.Metadata subject2 (HM.fromList [("random", random)])

    testData =
      [ (subject1, entry1)
      , (subject2, entry2)
      ]

  runIO $ traverse_ (uncurry write) testData

  with (pure $ webApp intf) $ do
    describe "GET /metadata/{subject}" $ do
      it "should return 404 if subject doesn't exist" $
        get "/metadata/bad"
          `shouldRespondWith`
            "Requested subject 'bad' not found" { matchStatus = 404 }

      it "should return the subject if it does exist" $
        get ("/metadata/" <> subject1Str)
          `shouldRespondWith`
            (matchingJSON entry1) { matchStatus = 200 }

    describe "GET /metadata/{subject}/properties" $ do
      it "should return 404 if subject doesn't exist" $
        get "/metadata/bad/properties"
          `shouldRespondWith`
            "Requested subject 'bad' not found" { matchStatus = 404 }

      it "should return the properties of the subject if it does exist" $
        get ("/metadata/" <> subject1Str <> "/properties")
          `shouldRespondWith`
            (matchingJSON entry1) { matchStatus = 200 }

    describe "GET /metadata/{subject}/properties/{property}" $ do
      it "should return 404 if subject doesn't exist" $
        get "/metadata/bad/properties/owner"
          `shouldRespondWith`
            "Requested subject 'bad' not found" { matchStatus = 404 }

      it "should return 404 if property doesn't exist" $
        get ("/metadata/" <> subject1Str <> "/properties/bad")
          `shouldRespondWith`
            "Requested subject '3' does not have the property 'bad'" { matchStatus = 404 }

      it "should return the property if it does exist" $ do
        get ("/metadata/" <> subject1Str <> "/properties/owner")
          `shouldRespondWith`
            (matchingJSON $ (Weakly.Metadata subject1 (HM.singleton "owner" owner))) { matchStatus = 200 }

        get ("/metadata/" <> subject1Str <> "/properties/subject")
          `shouldRespondWith`
            (matchingJSON $ Weakly.Metadata subject1 mempty) { matchStatus = 200 }

        get "/metadata/3/properties/odd"
          `shouldRespondWith`
            (matchingJSON (Weakly.Metadata subject1 (HM.singleton "odd" odd))) { matchStatus = 200 }

    describe "GET /metadata/query" $ do
      it "should return empty response if subject not found" $ do
        batchRequest (BatchRequest ["bad"] (Just []))
          `shouldRespondWith`
            (matchingJSON $ BatchResponse [])

        batchRequest (BatchRequest ["bad"] (Just ["owner"]))
          `shouldRespondWith`
            (matchingJSON $ BatchResponse [])

      it "should ignore subjects not found, returning subjects that were found" $
        batchRequest (BatchRequest [subject1, "bad"] (Just []))
          `shouldRespondWith`
            (matchingJSON $ BatchResponse [Weakly.Metadata "3" mempty])

      it "should return partial response if subject found but property not" $
        batchRequest (BatchRequest [subject1] (Just ["bad"]))
          `shouldRespondWith`
            (matchingJSON $ BatchResponse [Weakly.Metadata "3" mempty])

      it "should ignore properties not found, returning properties that were found" $
        batchRequest (BatchRequest [subject1] (Just ["owner", "bad"]))
          `shouldRespondWith`
            (matchingJSON $ BatchResponse [Weakly.Metadata "3" (HM.singleton "owner" owner)])

      it "should return a batch response" $ do
        batchRequest (BatchRequest [subject1] (Just ["owner", "subject"]))
          `shouldRespondWith`
            (matchingJSON $ BatchResponse [Weakly.Metadata "3" (HM.singleton "owner" owner)])

        batchRequest (BatchRequest [subject1, subject2] (Just ["owner", "subject"]))
          `shouldRespondWith`
            (matchingJSON $ BatchResponse [ Weakly.Metadata "3" (HM.singleton "owner" owner)
                                          , Weakly.Metadata "4" mempty
                                          ])

      it "should return all properties if key 'properties' not present in JSON request" $
        batchRequest (BatchRequest [subject1, subject2] Nothing)
          `shouldRespondWith`
            (matchingJSON $ BatchResponse
              [ entry1
              , entry2
              ])

batchRequest batchReq =
  request
    methodPost
    "/metadata/query"
    [ (hContentType, "application/json") ]
    (Aeson.encode $ batchReq)

matchingJSON :: ToJSON a => a -> ResponseMatcher
matchingJSON = fromString . BLC.unpack . Aeson.encode
