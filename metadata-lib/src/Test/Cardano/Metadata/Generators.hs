{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Test.Cardano.Metadata.Generators where

import qualified Cardano.Crypto.DSIGN.Class         as Crypto
import qualified Cardano.Crypto.DSIGN.Ed25519       as Crypto
import qualified Cardano.Crypto.Seed                as Crypto
import           Control.Monad.Except
import           Control.Monad.Morph                (hoist)
import qualified Data.Aeson                         as Aeson
import           Data.Aeson.TH
import           Data.Functor.Identity              (runIdentity)
import qualified Data.HashMap.Strict                as HM
import           Data.Int                           (Int32, Int64)
import           Data.List                          (intersperse)
import qualified Data.Map.Strict                    as M
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Vector                        as V
import           Data.Word
import           Hedgehog                           (Gen, MonadGen,
                                                     Opaque (Opaque), fromGenT)
import qualified Hedgehog.Gen                       as Gen
import qualified Hedgehog.Range                     as Range
import           Network.URI                        (URI (URI),
                                                     URIAuth (URIAuth))

import           Cardano.Metadata.Server.Types      (BatchRequest (BatchRequest),
                                                     BatchResponse (BatchResponse))
import           Cardano.Metadata.Transform         (Transform, mkTransform)
import           Cardano.Metadata.Types.Common      (AnnotatedSignature, AttestedProperty (AttestedProperty),
                                                     Description, File (File),
                                                     HashFn (Blake2b224, Blake2b256, SHA256),
                                                     Name, Owner (Owner),
                                                     PreImage (PreImage),
                                                     Property,
                                                     PropertyName (PropertyName),
                                                     PropertyType (Verifiable),
                                                     SequenceNumber,
                                                     Subject (Subject),
                                                     mkAnnotatedSignature,
                                                     seqFromNatural, unSubject)
import qualified Cardano.Metadata.Types.Weakly      as Weakly
import qualified Cardano.Metadata.Validation.GitHub as GitHub
import           Cardano.Metadata.Validation.Types  (Difference (Added, Changed, Removed))
import qualified Cardano.Metadata.Validation.Types  as Validation

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

sequenceNumber :: MonadGen m => m SequenceNumber
sequenceNumber = seqFromNatural <$> Gen.integral (Range.linear 0 (fromIntegral (maxBound :: Int32)))

attestedPropertySignedWith :: (MonadGen m, Aeson.ToJSON a) => Crypto.SignKeyDSIGN Crypto.Ed25519DSIGN -> Subject -> PropertyName -> m a -> m (AttestedProperty a)
attestedPropertySignedWith skey subj pName genA = do
  a <- genA
  AttestedProperty
    <$> pure a
    <*> (Gen.list (Range.linear 0 5) (pure $ mkAnnotatedSignature skey subj pName a))
    <*> sequenceNumber

attestedProperty' :: (MonadGen m, MonadIO m) => m (AttestedProperty Aeson.Value)
attestedProperty' =
  AttestedProperty
    <$> propertyValue
    <*> (Gen.list (Range.linear 0 5) annotatedSignature')
    <*> sequenceNumber

nameSignedWith :: MonadGen m => Crypto.SignKeyDSIGN Crypto.Ed25519DSIGN -> Subject -> m Name
nameSignedWith skey subj = attestedPropertySignedWith skey subj "name" (Gen.text (Range.linear 1 256) Gen.unicodeAll)

name :: (MonadIO m, MonadGen m) => m Name
name = do
  subj <- subject
  skey <- signingKey
  nameSignedWith skey subj

descriptionSignedWith :: MonadGen m => Crypto.SignKeyDSIGN Crypto.Ed25519DSIGN -> Subject -> m Description
descriptionSignedWith skey subj = attestedPropertySignedWith skey subj "description" (Gen.text (Range.linear 1 256) Gen.unicodeAll)

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

liftGen :: MonadGen m => Gen a -> m a
liftGen = fromGenT . hoist (pure . runIdentity)

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
batchResponse = BatchResponse <$> Gen.list (Range.linear 0 20) weaklyMetadata

key :: MonadGen m => m Word8
key = Gen.word8 (Range.linear 0 maxBound)

val :: MonadGen m => m Word8
val = Gen.word8 (Range.linear 0 maxBound)

keyVals :: MonadGen m => m [(Word8, Word8)]
keyVals = do
  Gen.list (Range.linear 0 20) ((,) <$> key <*> val)

verifiableProperty :: MonadGen m => m (Property 'Verifiable Aeson.Value)
verifiableProperty = propertyValue

weaklyMetadata :: MonadGen m => m (Weakly.Metadata)
weaklyMetadata = do
  Weakly.Metadata
  <$> subject
  <*> (fmap HM.fromList $ Gen.list (Range.linear 0 20) ((,) <$> propertyName <*> propertyValue))

validationMetadataSignedWith :: MonadGen m => Crypto.SignKeyDSIGN Crypto.Ed25519DSIGN -> Subject -> m Validation.Metadata
validationMetadataSignedWith skey subj = do
  let
    attestedPropTuple = do
      pName <- propertyName
      prop <- attestedPropertySignedWith skey subj pName propertyValue
      pure (pName, prop)

    verifiablePropTuple = do
      pName <- propertyName
      prop  <- verifiableProperty
      pure (pName, prop)

  Validation.Metadata subj
    <$> (fmap M.fromList $ Gen.list (Range.linear 0 10) attestedPropTuple)
    <*> (fmap M.fromList $ Gen.list (Range.linear 0 10) verifiablePropTuple)

validationMetadata :: (MonadIO m, MonadGen m) => Subject -> m Validation.Metadata
validationMetadata subj = do
  skey <- signingKey
  validationMetadataSignedWith skey subj

validationMetadata' :: (MonadIO m, MonadGen m) => m Validation.Metadata
validationMetadata' = do
  skey   <- signingKey
  subj   <- subject
  validationMetadataSignedWith skey subj

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

eitherWord8 :: MonadGen m => m (Either Word8 Word8)
eitherWord8 = Gen.either (Gen.word8 Range.constantBounded) (Gen.word8 Range.constantBounded)

transform :: MonadGen m => m (f a) -> m (Opaque (Transform r f a))
transform genFa = Gen.choice [ (Opaque . mkTransform . const) <$> genFa ]

diff :: MonadGen m => m a -> m (Difference a)
diff genA =
  Gen.choice [ Added <$> genA
             , Changed <$> genA <*> genA
             , Removed <$> genA
             ]

file :: MonadGen m => m a -> m (File a)
file genA =
  File
    <$> genA
    <*> Gen.integral (Range.exponential 0 (fromIntegral (maxBound :: Int64)))
    <*> Gen.string (Range.linear 0 64) Gen.unicode

gitHubFileStatus :: MonadGen m => m GitHub.GitHubFileStatus
gitHubFileStatus = Gen.choice [ pure GitHub.Added
                              , pure GitHub.Modified
                              , pure GitHub.Renamed
                              , pure GitHub.Removed
                              , GitHub.Unknown <$> Gen.text (Range.linear 0 64) Gen.unicode
                              ]

gitHubFile :: MonadGen m => m GitHub.GitHubFile
gitHubFile =
  GitHub.GitHubFile
  <$> Gen.text (Range.linear 0 64) Gen.unicode
  <*> gitHubFileStatus

gitHubPullRequest :: MonadGen m => m GitHub.GitHubPullRequest
gitHubPullRequest =
  GitHub.GitHubPullRequest
  <$> Gen.text (Range.linear 0 64) Gen.unicode
  <*> Gen.int (Range.linear 1 maxBound)

$(deriveJSON defaultOptions ''ComplexType)
