{-# LANGUAGE OverloadedStrings #-}

module Test.Generators where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import           Data.Word
import           Hedgehog (Gen) 
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog
import           Data.Text (Text)
import qualified Data.Aeson as Aeson

import Cardano.Metadata.Server.Types

hashFn :: Gen HashFn
hashFn = Gen.choice [ pure Blake2b256
                    , pure Blake2b224
                    , pure SHA256
                    ]

publicKey :: Gen Text
publicKey = Gen.text (Range.linear 0 64) Gen.hexit

sig :: Gen Text
sig = Gen.text (Range.linear 0 128) Gen.hexit

annotatedSignature :: Gen AnnotatedSignature
annotatedSignature = AnnotatedSignature <$> publicKey <*> sig

name :: Gen Text
name = Gen.choice [ pure "description"
                  , pure "name"
                  , Gen.text (Range.linear 0 128) Gen.unicodeAll
                  ]

subject :: Gen Subject
subject = Gen.text (Range.linear 0 128) Gen.unicodeAll

metadataValue :: Gen Text
metadataValue = Gen.text (Range.linear 0 128) Gen.unicodeAll

metadataProperty :: Gen Property
metadataProperty = Property <$> metadataValue <*> Gen.list (Range.linear 0 25) annotatedSignature

preImage :: Gen PreImage
preImage = PreImage <$> metadataValue <*> hashFn

owner :: Gen Owner
owner = Owner <$> publicKey <*> sig

entry :: Gen Entry
entry = Entry <$> subject <*> owner <*> metadataProperty <*> metadataProperty <*> preImage

batchRequest :: Gen BatchRequest
batchRequest =
  BatchRequest
    <$> Gen.list (Range.linear 0 20) subject
    <*> Gen.list (Range.linear 0 10) name

anyProperty :: Gen AnyProperty
anyProperty = Gen.choice [ PropertyPreImage <$> preImage
                         , PropertyOwner <$> owner
                         , PropertyGeneric <$> name <*> metadataProperty
                         ]

partialEntry :: Gen PartialEntry
partialEntry = do
  PartialEntry
    <$> subject
    <*> (HM.fromList <$> Gen.list (Range.linear 0 10) anyPropertyWithKey)

anyPropertyWithKey :: Gen (Text, AnyProperty)
anyPropertyWithKey = do
  prop <- anyProperty
  pure (anyPropertyJSONKey prop, prop)

batchResponse :: Gen BatchResponse
batchResponse = BatchResponse <$> Gen.list (Range.linear 0 20) partialEntry
