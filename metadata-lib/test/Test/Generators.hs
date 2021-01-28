{-# LANGUAGE OverloadedStrings #-}

module Test.Generators where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Data.Functor.Identity
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import           Data.Word
import           Hedgehog (Gen, MonadGen) 
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog
import           Data.Text (Text)
import qualified Data.Aeson as Aeson

import Cardano.Metadata.Server.Types
import Cardano.Metadata.Store.Types

hashFn :: MonadGen m => m HashFn
hashFn = Gen.choice [ pure Blake2b256
                    , pure Blake2b224
                    , pure SHA256
                    ]

publicKey :: MonadGen m => m Text
publicKey = Gen.text (Range.linear 0 64) Gen.hexit

sig :: MonadGen m => m Text
sig = Gen.text (Range.linear 0 128) Gen.hexit

annotatedSignature :: MonadGen m => m AnnotatedSignature
annotatedSignature = AnnotatedSignature <$> publicKey <*> sig

name :: MonadGen m => m Text
name = Gen.choice [ pure "description"
                  , pure "name"
                  , Gen.text (Range.linear 0 128) Gen.unicodeAll
                  ]

subject :: MonadGen m => m Subject
subject = Gen.text (Range.linear 0 128) Gen.unicodeAll

metadataValue :: MonadGen m => m Text
metadataValue = Gen.text (Range.linear 0 128) Gen.unicodeAll

metadataProperty :: MonadGen m => m Property
metadataProperty = Property <$> metadataValue <*> Gen.list (Range.linear 0 25) annotatedSignature

preImage :: MonadGen m => m PreImage
preImage = PreImage <$> metadataValue <*> hashFn

owner :: MonadGen m => m Owner
owner = Owner <$> publicKey <*> sig

entry :: MonadGen m => m Entry
entry = Entry
  <$> (EntryF
        <$> subject
        <*> (Identity <$> owner)
        <*> (Identity <$> metadataProperty)
        <*> (Identity <$> metadataProperty)
        <*> (Identity <$> preImage))

batchRequest :: MonadGen m => m BatchRequest
batchRequest =
  BatchRequest
    <$> Gen.list (Range.linear 0 20) subject
    <*> Gen.list (Range.linear 0 10) name

batchRequestFor :: [Subject] -> Gen BatchRequest
batchRequestFor subjects = do
  subjs <- Gen.list (Range.linear 1 (length subjects)) $ Gen.choice (pure <$> subjects)
  props <- Gen.list (Range.linear 0 (length availablePropertyNames)) $ Gen.choice (pure <$> availablePropertyNames)
  pure $ BatchRequest subjs props

partialEntry :: MonadGen m => m PartialEntry
partialEntry = do
  PartialEntry <$>
    (EntryF
    <$> subject
    <*> Gen.maybe owner
    <*> Gen.maybe metadataProperty
    <*> Gen.maybe metadataProperty
    <*> Gen.maybe preImage
    )

batchResponse :: MonadGen m => m BatchResponse
batchResponse = BatchResponse <$> Gen.list (Range.linear 0 20) partialEntry

key :: MonadGen m => m Word8
key = Gen.word8 (Range.linear 0 maxBound)

val :: MonadGen m => m Word8
val = Gen.word8 (Range.linear 0 maxBound)

store :: (MonadGen m, MonadIO m) => StoreInterface Word8 Word8 -> m ()
store (StoreInterface _ write delete _ _) = do
  k <- key
  v <- val

  liftIO $ write k v
  
