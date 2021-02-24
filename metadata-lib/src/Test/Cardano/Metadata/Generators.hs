{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Metadata.Generators where

import           Control.Monad.Except
import qualified Data.Aeson                    as Aeson
import           Data.Aeson.TH
import           Data.ByteArray.Encoding       (Base (Base64), convertToBase)
import qualified Data.ByteString               as BS
import qualified Data.HashMap.Strict           as HM
import           Data.List                     (intersperse)
import qualified Data.Map.Strict               as M
import           Data.Text                     (Text)
import  Data.Functor.Identity (runIdentity)
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           Data.Word
import           Hedgehog                      (Gen, MonadGen, fromGenT)
import Control.Monad.Morph (hoist)
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Network.URI                   (URI (URI), URIAuth (URIAuth))
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Seed as Crypto
import qualified Cardano.Crypto.DSIGN.Ed25519 as Crypto
import qualified Cardano.Api as Cardano
import qualified Test.Cardano.Api.Typed.Gen as Cardano

import           Cardano.Metadata.Server.Types (BatchRequest (BatchRequest),
                                                BatchResponse (BatchResponse))
import           Cardano.Metadata.Types.Common (AnnotatedSignature, 
                                                Description, Encoded (Encoded),
                                                HashFn (Blake2b224, Blake2b256, SHA256),
                                                Name, Owner (Owner),
                                                PreImage (PreImage),
                                                Property (Property),
                                                PropertyName (PropertyName),
                                                Subject (Subject), unSubject, mkAnnotatedSignature)
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

signingKey :: MonadIO m => m (Crypto.SignKeyDSIGN Crypto.Ed25519DSIGN)
signingKey = liftIO $ do
  seed <- Crypto.readSeedFromSystemEntropy 32
  pure $ Crypto.genKeyDSIGN seed

annotatedSignature' :: (MonadGen m, MonadIO m) => m AnnotatedSignature
annotatedSignature' = do
  subj  <- subject
  pName <- propertyName
  v     <- propertyValue
  skey  <- signingKey

  pure $ mkAnnotatedSignature skey subj pName v

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
preImage = PreImage <$> Gen.text (Range.linear 0 128) Gen.hexit <*> hashFn

owner :: MonadGen m => m Owner
owner = Owner <$> publicKey <*> sig

stronglyTypedPropertySignedWith :: (MonadGen m, Aeson.ToJSON a) => Crypto.SignKeyDSIGN Crypto.Ed25519DSIGN -> Subject -> PropertyName -> m a -> m (Property a)
stronglyTypedPropertySignedWith skey subj pName genA = do
  a <- genA
  Property <$> pure a <*> (Gen.maybe $ Gen.list (Range.linear 0 5) (pure $ mkAnnotatedSignature skey subj pName a))

nameSignedWith :: MonadGen m => Crypto.SignKeyDSIGN Crypto.Ed25519DSIGN -> Subject -> m Name
nameSignedWith skey subj = stronglyTypedPropertySignedWith skey subj "name" (Gen.text (Range.linear 1 256) Gen.unicodeAll)

name :: (MonadIO m, MonadGen m) => m Name
name = do
  subj <- subject
  skey <- signingKey
  nameSignedWith skey subj

descriptionSignedWith :: MonadGen m => Crypto.SignKeyDSIGN Crypto.Ed25519DSIGN -> Subject -> m Description
descriptionSignedWith skey subj = stronglyTypedPropertySignedWith skey subj "description" (Gen.text (Range.linear 1 256) Gen.unicodeAll)

description :: (MonadIO m, MonadGen m) => m Description
description = do
  subj <- subject
  skey <- signingKey
  descriptionSignedWith skey subj

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

ticker :: MonadGen m => m Wallet.Ticker
ticker = Wallet.Ticker <$> Gen.text (Range.linear 2 4) Gen.unicodeAll

assetLogo :: MonadGen m => m Wallet.AssetLogo
assetLogo = Wallet.AssetLogo . Encoded . convertToBase Base64 . BS.pack <$> Gen.list (Range.linear 0 256) (Gen.word8 Range.constantBounded)

assetUnit :: MonadGen m => m Wallet.AssetUnit
assetUnit = Wallet.AssetUnit
  <$> Gen.text (Range.linear 1 30) Gen.unicodeAll
  <*> Gen.integral (Range.linear 1 19)

liftGen :: MonadGen m => Gen a -> m a
liftGen = fromGenT . hoist (pure . runIdentity) 

policy :: MonadGen m => m Wallet.Policy
policy = do
  script <- liftGen $ Cardano.genScriptInEra Cardano.MaryEra

  pure $ Wallet.mkPolicy script

walletMetadataSignedWith :: MonadGen m => Crypto.SignKeyDSIGN Crypto.Ed25519DSIGN -> Subject -> Wallet.Policy -> m Wallet.Metadata
walletMetadataSignedWith skey subj pol = do
  let
    weakProp = do
      pName <- propertyName 
      prop  <- weaklyTypedPropertySignedWith skey subj pName
      pure (pName, prop)

  n      <- stronglyTypedPropertySignedWith skey subj "name" (Gen.text (Range.linear 1 50) Gen.unicodeAll)
  desc   <- descriptionSignedWith skey subj
  unit   <- Gen.maybe (stronglyTypedPropertySignedWith skey subj "unit" assetUnit)
  logo   <- Gen.maybe (stronglyTypedPropertySignedWith skey subj "logo" assetLogo)
  url    <- Gen.maybe (stronglyTypedPropertySignedWith skey subj "url" assetURL)
  tick   <- Gen.maybe (stronglyTypedPropertySignedWith skey subj "ticker" ticker)
  rest   <- (fmap HM.fromList $ Gen.list (Range.linear 1 5) weakProp)

  pure $ Wallet.Metadata subj pol n desc unit logo url tick rest

walletMetadata :: (MonadGen m, MonadIO m) => Subject -> Wallet.Policy -> m Wallet.Metadata
walletMetadata subj pol = do
  skey <- signingKey
  walletMetadataSignedWith skey subj pol

walletMetadata' :: (MonadGen m, MonadIO m) => m Wallet.Metadata
walletMetadata' = do
  pol <- policy
  subj <- (Wallet.policyId pol <>) <$> Gen.text (Range.linear 0 200) (Gen.unicodeAll)
  walletMetadata (Subject subj) pol

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

batchResponse :: (MonadIO m, MonadGen m) => m BatchResponse
batchResponse = BatchResponse <$> Gen.list (Range.linear 0 20) weaklyTypedMetadata'

key :: MonadGen m => m Word8
key = Gen.word8 (Range.linear 0 maxBound)

val :: MonadGen m => m Word8
val = Gen.word8 (Range.linear 0 maxBound)

keyVals :: MonadGen m => m [(Word8, Word8)]
keyVals = do
  Gen.list (Range.linear 0 20) ((,) <$> key <*> val)

weaklyTypedPropertySignedWith :: MonadGen m => Crypto.SignKeyDSIGN Crypto.Ed25519DSIGN -> Subject -> PropertyName -> m Weakly.Property
weaklyTypedPropertySignedWith skey subj pName = do
  v <- propertyValue
  Property <$> pure v <*> (Gen.maybe $ Gen.list (Range.linear 0 5) (pure $ mkAnnotatedSignature skey subj pName v))

weaklyTypedProperty :: (MonadIO m, MonadGen m) => Subject -> PropertyName -> m Weakly.Property
weaklyTypedProperty subj pName = do
  skey  <- signingKey
  weaklyTypedPropertySignedWith skey subj pName

weaklyTypedProperty' :: (MonadIO m, MonadGen m) => m Weakly.Property
weaklyTypedProperty' = do
  subj  <- subject
  pName <- propertyName
  skey  <- signingKey
  weaklyTypedPropertySignedWith skey subj pName

weaklyTypedMetadataSignedWith :: MonadGen m => Crypto.SignKeyDSIGN Crypto.Ed25519DSIGN -> Subject -> m Weakly.Metadata
weaklyTypedMetadataSignedWith skey subj = do
  let
    weakProp = do
      pName <- propertyName
      prop  <- weaklyTypedPropertySignedWith skey subj pName
      pure (pName, prop)

  Weakly.Metadata subj <$> (fmap HM.fromList $ Gen.list (Range.linear 0 20) weakProp)

weaklyTypedMetadata :: (MonadIO m, MonadGen m) => Subject -> m Weakly.Metadata
weaklyTypedMetadata subj = do
  skey <- signingKey
  weaklyTypedMetadataSignedWith skey subj 

weaklyTypedMetadata' :: (MonadIO m, MonadGen m) => m Weakly.Metadata
weaklyTypedMetadata' = do
  skey   <- signingKey
  subj   <- subject
  weaklyTypedMetadataSignedWith skey subj 

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
