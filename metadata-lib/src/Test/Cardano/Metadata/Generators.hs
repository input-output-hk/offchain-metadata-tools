{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Cardano.Metadata.Generators where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import qualified Data.Aeson                    as Aeson
import           Data.Aeson.TH
import           Data.ByteArray.Encoding       (Base (Base16, Base64),
                                                convertFromBase, convertToBase)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base64        as Base64
import           Data.Functor.Identity
import qualified Data.HashMap.Strict           as HM
import           Data.List                     (intersperse)
import qualified Data.Map.Strict               as M
import           Data.Monoid                   (First (First))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           Data.Word
import           Hedgehog                      (Gen, MonadGen)
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Network.URI                   (URI (URI), URIAuth (URIAuth))
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.Hedgehog

import           Cardano.Metadata.Server.Types (BatchRequest (BatchRequest),
                                                BatchResponse (BatchResponse))
import           Cardano.Metadata.Store.Types
import           Cardano.Metadata.Types.Common (AnnotatedSignature (AnnotatedSignature),
                                                Description, Encoded (Encoded),
                                                HashFn (Blake2b224, Blake2b256, SHA256),
                                                Name, Owner (Owner),
                                                PreImage (PreImage),
                                                Property (Property),
                                                PropertyName (PropertyName),
                                                Subject (Subject), unSubject)
import qualified Cardano.Metadata.Types.Wallet as Wallet
import qualified Cardano.Metadata.Types.Weakly as Weakly

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
complexKey = unSubject <$> subject

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
                      , fmap (PropertyName) $ Gen.text (Range.linear 0 128) Gen.unicodeAll
                      ]

subject :: MonadGen m => m Subject
subject = Subject <$> Gen.text (Range.linear 1 256) Gen.unicodeAll

metadataValue :: MonadGen m => m Text
metadataValue = Gen.text (Range.linear 0 128) Gen.unicodeAll

preImage :: MonadGen m => m PreImage
preImage = PreImage <$> metadataValue <*> hashFn

owner :: MonadGen m => m Owner
owner = Owner <$> publicKey <*> sig

stronglyTypedProperty :: MonadGen m => m a -> m (Property a)
stronglyTypedProperty genA =
  Property <$> genA <*> Gen.list (Range.linear 0 5) annotatedSignature

name :: MonadGen m => m Name
name = stronglyTypedProperty (Gen.text (Range.linear 1 256) Gen.unicodeAll)

description :: MonadGen m => m Description
description = stronglyTypedProperty (Gen.text (Range.linear 1 256) Gen.unicodeAll)

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

assetURL :: MonadGen m => m Wallet.AssetURL
assetURL = Wallet.AssetURL <$> httpsURI

acronym :: MonadGen m => m Wallet.Acronym
acronym = Wallet.Acronym <$> Gen.text (Range.linear 2 4) Gen.unicodeAll

assetLogo :: MonadGen m => m Wallet.AssetLogo
assetLogo = Wallet.AssetLogo . Encoded . convertToBase Base64 . BS.pack <$> Gen.list (Range.linear 0 256) (Gen.word8 Range.constantBounded)

assetUnit :: MonadGen m => m Wallet.AssetUnit
assetUnit = Wallet.AssetUnit
  <$> Gen.text (Range.linear 1 30) Gen.unicodeAll
  <*> Gen.integral (Range.linear 1 19)

walletMetadata :: MonadGen m => m Wallet.Metadata
walletMetadata = Wallet.Metadata
  <$> subject
  <*> stronglyTypedProperty (Gen.text (Range.linear 1 50) Gen.unicodeAll)
  <*> description
  <*> Gen.maybe (stronglyTypedProperty assetUnit)
  <*> Gen.maybe (stronglyTypedProperty assetLogo)
  <*> Gen.maybe (stronglyTypedProperty assetURL)
  <*> Gen.maybe (stronglyTypedProperty acronym)
  <*> (fmap HM.fromList $ Gen.list (Range.linear 1 5) ((,) <$> propertyName <*> weaklyTypedProperty))

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

batchResponse :: MonadGen m => m BatchResponse
batchResponse = BatchResponse <$> Gen.list (Range.linear 0 20) weaklyTypedMetadata

key :: MonadGen m => m Word8
key = Gen.word8 (Range.linear 0 maxBound)

val :: MonadGen m => m Word8
val = Gen.word8 (Range.linear 0 maxBound)

keyVals :: MonadGen m => m [(Word8, Word8)]
keyVals = do
  Gen.list (Range.linear 0 20) ((,) <$> key <*> val)

weaklyTypedProperty :: MonadGen m => m Weakly.Property
weaklyTypedProperty = Property <$> propertyValue <*> Gen.list (Range.linear 0 5) annotatedSignature

weaklyTypedMetadata :: MonadGen m => m Weakly.Metadata
weaklyTypedMetadata =
  Weakly.Metadata
  <$> subject
  <*> (fmap HM.fromList $ Gen.list (Range.linear 0 20) ((,) <$> propertyName <*> weaklyTypedProperty))

propertyName :: MonadGen m => m PropertyName
propertyName = PropertyName <$> Gen.text (Range.linear 1 64) Gen.unicodeAll

propertyValue :: MonadGen m => m Aeson.Value
propertyValue =
  Gen.recursive Gen.choice
    [ Aeson.String <$> Gen.text (Range.linear 1 64) Gen.unicodeAll
    , Aeson.Number <$> fromIntegral <$> Gen.word8 Range.constantBounded
    , Aeson.Bool <$> Gen.bool
    , pure $ Aeson.Null
    ]
    [ Aeson.Array . V.fromList <$> Gen.list (Range.linear 0 5) propertyValue
    , Aeson.Object . HM.fromList <$> Gen.list (Range.linear 0 5) ((,) <$> Gen.text (Range.linear 1 64) Gen.unicodeAll <*> propertyValue)
    ]

$(deriveJSON defaultOptions ''ComplexType)
