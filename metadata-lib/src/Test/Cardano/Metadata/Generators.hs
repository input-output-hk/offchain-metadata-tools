{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Metadata.Generators where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Data.Functor.Identity
import           Data.Text (Text)
import           Data.Word
import           Hedgehog (Gen, MonadGen) 
import Data.ByteArray.Encoding
    ( Base (Base16, Base64), convertFromBase, convertToBase )
import           Network.URI (URI(URI), URIAuth(URIAuth))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog
import Data.Aeson.TH
import Data.List (intersperse)
import Data.Monoid (First(First))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Metadata.Server.Types
import Cardano.Metadata.Store.Types

data ComplexType = ComplexType { _ctArr :: [Int]
                               , _ctMap :: M.Map Word8 Word8
                               }
  deriving (Eq, Show)

type ComplexKey = Text

complexType :: MonadGen m => m ComplexType
complexType =
  ComplexType
  <$> Gen.list (Range.linear 0 20) (Gen.int (Range.linear 0 maxBound))
  <*> Gen.map (Range.linear 0 20) ((,) <$> key <*> val)

complexKey :: MonadGen m => m ComplexKey
complexKey = subject

complexKeyVals :: MonadGen m => m [(ComplexKey, ComplexType)]
complexKeyVals = Gen.list (Range.linear 0 20) ((,) <$> complexKey <*> complexType)

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

propName :: MonadGen m => m PropertyName
propName = Gen.choice [ pure $ PropertyName "description"
                          , pure $ PropertyName "name"
                          , pure $ PropertyName "name"
                          , fmap (PropertyName) $ Gen.text (Range.linear 0 128) Gen.unicodeAll
                          ]

subject :: MonadGen m => m Subject
subject = Gen.text (Range.linear 0 128) Gen.unicodeAll

metadataValue :: MonadGen m => m Text
metadataValue = Gen.text (Range.linear 0 128) Gen.unicodeAll

metadataProperty :: MonadGen m => m value -> m (GenericProperty value)
metadataProperty genValue = GenericProperty <$> genValue <*> Gen.list (Range.linear 0 25) annotatedSignature

preImage :: MonadGen m => m PreImage
preImage = PreImage <$> metadataValue <*> hashFn

owner :: MonadGen m => m Owner
owner = Owner <$> publicKey <*> sig

httpsURI :: MonadGen m => m URI
httpsURI = (URI <$> pure "https:" <*> (Just <$> uriAuthority) <*> (T.unpack <$> uriPath) <*> pure mempty <*> pure mempty)

uriPath :: MonadGen m => m Text
uriPath = do
  xs <- Gen.list (Range.linear 0 10) (Gen.text (Range.linear 0 10) Gen.alphaNum)
  pure $ mconcat $ "/":(intersperse "/" xs)

uriAuthority :: MonadGen m => m URIAuth
uriAuthority = do
  mid <- Gen.text (Range.linear 1 64) Gen.alphaNum
  end <- Gen.choice [ pure ".org"
                    , pure ".com"
                    , pure ".net"
                    , pure ".com.au"
                    , pure ".io"
                    , pure ".tld"
                    ]
  pure $ URIAuth mempty (T.unpack $ "www." <> mid <> end) mempty

assetURL :: MonadGen m => m AssetURL
assetURL = AssetURL <$> httpsURI

acronym :: MonadGen m => m Acronym
acronym = Gen.text (Range.linear 1 4) Gen.unicodeAll

assetLogo :: MonadGen m => m AssetLogo
assetLogo = AssetLogo . Encoded . convertToBase Base64 . BS.pack <$> Gen.list (Range.linear 0 256) (Gen.word8 Range.constantBounded)

assetUnit :: MonadGen m => m AssetUnit
assetUnit = AssetUnit
  <$> Gen.text (Range.linear 1 30) Gen.unicodeAll
  <*> Gen.integral (Range.linear 0 19)

entry :: MonadGen m => m Entry
entry = Entry
  <$> metadataProperty metadataValue
  <*> metadataProperty metadataValue
  <*> Gen.maybe owner
  <*> Gen.maybe (metadataProperty acronym)
  <*> Gen.maybe (metadataProperty assetURL)
  <*> Gen.maybe (metadataProperty assetLogo)
  <*> Gen.maybe (metadataProperty assetUnit)

batchRequest :: MonadGen m => m BatchRequest
batchRequest =
  BatchRequest
    <$> Gen.list (Range.linear 0 20) subject
    <*> Gen.maybe (Gen.list (Range.linear 0 10) propName)

batchRequestFor :: [Subject] -> Gen BatchRequest
batchRequestFor subjects = do
  subjs <- Gen.list (Range.linear 1 (length subjects)) $ Gen.choice (pure <$> subjects)
  props <- Gen.maybe $ Gen.list (Range.linear 0 20) propName
  pure $ BatchRequest subjs props

partialEntry :: MonadGen m => m PartialEntry
partialEntry = do
  PartialEntry <$>
    (EntryF
    <$> (First <$> Gen.maybe owner)
    <*> (First <$> Gen.maybe (metadataProperty metadataValue))
    <*> (First <$> Gen.maybe (metadataProperty metadataValue))
    <*> (First <$> Gen.maybe (metadataProperty acronym))
    <*> (First <$> Gen.maybe (metadataProperty assetURL))
    <*> (First <$> Gen.maybe (metadataProperty assetLogo))
    <*> (First <$> Gen.maybe (metadataProperty assetUnit))
    )

batchResponse :: MonadGen m => m BatchResponse
batchResponse = BatchResponse <$> Gen.list (Range.linear 0 20) (PartialEntry' <$> subject <*> partialEntry)

key :: MonadGen m => m Word8
key = Gen.word8 (Range.linear 0 maxBound)

val :: MonadGen m => m Word8
val = Gen.word8 (Range.linear 0 maxBound)

-- store :: MonadGen m => StoreInterface Word8 Word8 kvs -> m (kvs -> IO kvs)
-- store (StoreInterface _ write delete _ _) = do
--   k <- key
--   v <- val

--   pure $ write k v
  
-- storeWrites :: MonadGen m => m [StoreOperation Word8 Word8 ()]
-- storeWrites = do
--   k <- key
--   v <- val

--   pure [StoreWrite k v]

keyVals :: MonadGen m => m [(Word8, Word8)]
keyVals = do
  Gen.list (Range.linear 0 20) ((,) <$> key <*> val)

$(deriveJSON defaultOptions ''ComplexType)
