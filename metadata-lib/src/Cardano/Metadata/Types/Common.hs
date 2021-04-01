{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Metadata.Types.Common where

import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash
import Control.DeepSeq
    ( NFData )
import Data.Aeson
    ( FromJSON, FromJSONKey, ToJSON, ToJSONKey, (.:), (.:?) )
import qualified Data.Aeson as Aeson
import Data.Aeson.TH
    ( deriveJSON )
import qualified Data.Aeson.Types as Aeson
import Data.ByteArray.Encoding
    ( Base (Base16, Base64), convertFromBase, convertToBase )
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable
    ( Hashable )
import qualified Data.HashMap.Strict as HM
import Data.Int
    ( Int64 )
import Data.Maybe
    ( fromMaybe )
import Data.Scientific
    ( toBoundedInteger )
import Data.String
    ( IsString )
import Data.Text
    ( Text )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (Quiet) )
import System.FilePath.Posix
    ( takeBaseName, takeExtensions )
import Text.Casing
    ( fromHumps, toCamel )
import Text.ParserCombinators.ReadP
    ( choice, string )
import Text.Read
    ( readEither, readPrec )
import qualified Text.Read as Read
    ( lift )
import Web.HttpApiData
    ( FromHttpApiData )

-- | The metadata subject, the on-chain identifier
newtype Subject = Subject { unSubject :: Text }
  deriving (Generic, Eq, Ord, FromHttpApiData, ToJSONKey, FromJSONKey, ToJSON, FromJSON, Hashable)
  deriving newtype (IsString)
  deriving (Show) via (Quiet Subject)

newtype SequenceNumber = SequenceNumber { unSequenceNumber :: Natural }
  deriving (Generic, Eq, Ord, Hashable)
  deriving (Show) via (Quiet SequenceNumber)

seqFromIntegral :: Integral n => n -> Maybe SequenceNumber
seqFromIntegral x | x < 0 = Nothing
seqFromIntegral x = Just $ SequenceNumber (fromInteger (toInteger x))

seqFromNatural :: Natural -> SequenceNumber
seqFromNatural n = SequenceNumber n

seqToInteger :: SequenceNumber -> Integer
seqToInteger (SequenceNumber n) = toInteger n

seqZero :: SequenceNumber
seqZero = SequenceNumber 0

seqSucc :: SequenceNumber -> SequenceNumber
seqSucc (SequenceNumber n) = SequenceNumber (succ n)

seqPred :: SequenceNumber -> SequenceNumber
seqPred (SequenceNumber n) = SequenceNumber (max 0 (pred n))

newtype PropertyName = PropertyName { unPropertyName :: Text }
  deriving (Generic, Eq, Ord, FromHttpApiData, ToJSONKey, FromJSONKey, ToJSON, FromJSON, Hashable)
  deriving newtype (IsString)
  deriving (Show) via (Quiet PropertyName)

data AttestedProperty a
  = AttestedProperty { attestedValue          :: a
                     , attestedSignatures     :: [AnnotatedSignature]
                     , attestedSequenceNumber :: SequenceNumber
                     }
  deriving (Eq, Show)

data PropertyType = Verifiable
                  | Attested

type family Property (propertyType :: PropertyType) a where
  Property 'Verifiable a = a
  Property 'Attested   a = AttestedProperty a

toPropertyNameList :: [(Text, a)] -> [(PropertyName, a)]
toPropertyNameList = fmap (\(k, v) -> (PropertyName k, v))

fromPropertyNameList :: [(PropertyName, a)] -> [(Text, a)]
fromPropertyNameList = fmap (\(k, v) -> (unPropertyName k, v))


-- | A human-readable name for the metadata subject, suitable for use in an interface
type Name        = Property 'Attested Text

-- | A human-readable description for the metadata subject, suitable for use in an interface
type Description = Property 'Attested Text

-- | A pair of a public key, and a signature of the metadata entry by
-- that public key.
data AnnotatedSignature =
  AnnotatedSignature { asAttestationSignature :: SigDSIGN Ed25519DSIGN
                     , asPublicKey            :: VerKeyDSIGN Ed25519DSIGN
                     }
  deriving (Eq, Show)

-- | Hash functions supported by 'PreImage'.
data HashFn = Blake2b256
            | Blake2b224
            | SHA256
  deriving (Eq)

-- | A pair of a hash function identifier and a bytestring, such that
-- the bytestring is the preimage of the metadata subject under that hash
-- function.
data PreImage
  = PreImage { piValue  :: Text
             , piHashFn :: HashFn
             }
  deriving (Eq, Show)

-- | Public key and signature attesting to ownership of the metadata
-- entry in this registry.
data Owner
  = Owner { ownSignature :: Text
          , ownPublicKey :: Text
          }
  deriving (Eq, Show)

newtype Encoded (base :: Base) = Encoded
    { rawEncoded :: ByteString }
    deriving (Generic, Show, Eq, Ord, Semigroup, Monoid)

mkAnnotatedSignature :: forall val . ToJSON val => SignKeyDSIGN Ed25519DSIGN -> Subject -> PropertyName -> val -> AnnotatedSignature
mkAnnotatedSignature skey subj propName propVal =
  let
    hashSubj     = hashWith (T.encodeUtf8 . unSubject) subj :: Hash Blake2b_256 Subject
    hashPropName = hashWith (T.encodeUtf8 . unPropertyName) propName :: Hash Blake2b_256 PropertyName
    hashPropVal  = hashWith (BSL.toStrict . Aeson.encode) propVal :: Hash Blake2b_256 val

    h = hashWith id
      (  hashToBytes hashSubj
      <> hashToBytes hashPropName
      <> hashToBytes hashPropVal
      ) :: Hash Blake2b_256 ByteString
    publicKey = deriveVerKeyDSIGN skey
    sig       = signDSIGN () (hashToBytes h) skey
  in
    AnnotatedSignature sig publicKey

deserialiseBase16 :: Text -> Either Text ByteString
deserialiseBase16 t =
  case (convertFromBase Base16 . T.encodeUtf8 $ t) of
    Left err -> Left . T.pack $ "Failed to deserialise Base16 bytestring from text: '" <> T.unpack t <> "', error was: " <> err
    Right x  -> pure x

deserialiseAttestationSignature :: ByteString -> Either Text (SigDSIGN Ed25519DSIGN)
deserialiseAttestationSignature t =
  case rawDeserialiseSigDSIGN t of
    Nothing -> Left . T.pack $ "Failed to parse Ed25519DSIGN signature from '" <> BC.unpack t <> "'."
    Just x  -> pure x

deserialisePublicKey :: ByteString -> Either Text (VerKeyDSIGN Ed25519DSIGN)
deserialisePublicKey t =
  case rawDeserialiseVerKeyDSIGN t of
    Nothing -> Left . T.pack $ "Failed to parse Ed25519DSIGN verification key from '" <> BC.unpack t <> "'."
    Just x  -> pure x

data File a
  = File { fileContents :: a
         , fileSize     :: Natural
         , filePath     :: FilePath
         }
  deriving (Eq, Show)

-- | Get the base name of the file, without extenstion or path
--
-- @
-- fileBaseName "/directory/file.ext" == "file"
-- @
fileBaseName :: File a -> Text
fileBaseName = T.pack . takeBaseName . filePath

-- | Get the extensions of a file.
--
-- @
-- fileExtensions "/directory/path.ext" == ".ext"
-- fileExtensions "file.tar.gz" == ".tar.gz"
-- @
fileExtensions :: File a -> Text
fileExtensions = T.pack . takeExtensions . filePath

-- Instances

instance FromJSON (Encoded 'Base16) where
  parseJSON = Aeson.withText "base16 bytestring" $
      either fail (pure . Encoded) . convertFromBase Base16 . T.encodeUtf8

instance ToJSON (Encoded 'Base16) where
  toJSON (Encoded raw) = Aeson.String $ T.decodeUtf8 $ convertToBase Base16 raw

instance FromJSON (Encoded 'Base64) where
  parseJSON = Aeson.withText "base64 bytestring" $
      either fail (pure . Encoded) . convertFromBase Base64 . T.encodeUtf8

instance ToJSON (Encoded 'Base64) where
  toJSON (Encoded raw) = Aeson.String $ T.decodeUtf8 $ convertToBase Base64 raw

instance NFData (Encoded 'Base16)
instance NFData (Encoded 'Base64)

instance ToJSON HashFn where
  toJSON = Aeson.String . T.pack . show

instance FromJSON HashFn where
  parseJSON = Aeson.withText "HashFn" (either Aeson.parseFail pure . readEither . T.unpack)

instance Show HashFn where
  show Blake2b256 = "blake2b-256"
  show Blake2b224 = "blake2b-224"
  show SHA256     = "sha256"

instance Read HashFn where
  readPrec = Read.lift $ choice [ Blake2b256 <$ string "blake2b-256"
                                , Blake2b224 <$ string "blake2b-224"
                                , SHA256     <$ string "sha256"
                                ]

instance Aeson.ToJSON v => Aeson.ToJSON (AttestedProperty v) where
  toJSON (AttestedProperty v sigs sequenceNumber) =
    Aeson.Object . HM.fromList $
      [ ("value", Aeson.toJSON v)
      , ("signatures", Aeson.toJSON sigs)
      , ("sequenceNumber", Aeson.toJSON sequenceNumber)
      ]

instance Aeson.FromJSON v => Aeson.FromJSON (AttestedProperty v) where
  parseJSON = Aeson.withObject "AttestedProperty" $ \obj ->
    AttestedProperty
    <$> obj .: "value"
    <*> (fromMaybe [] <$> obj .:? "signatures")
    <*> obj .: "sequenceNumber"

instance ToJSON AnnotatedSignature where
  toJSON (AnnotatedSignature sig pubKey) = Aeson.Object . HM.fromList $
    [ ("signature", Aeson.String $ T.decodeUtf8 $ convertToBase Base16 $ rawSerialiseSigDSIGN sig)
    , ("publicKey", Aeson.String $ T.decodeUtf8 $ convertToBase Base16 $ rawSerialiseVerKeyDSIGN pubKey)
    ]

instance FromJSON AnnotatedSignature where
  parseJSON = Aeson.withObject "AnnotatedSignature" $ \obj -> do
    AnnotatedSignature
    <$> (deserialiseSigDSIGN' =<< deserialiseBase16' =<< obj .: "signature")
    <*> (deserialiseVerKeyDSIGN' =<< deserialiseBase16' =<< obj .: "publicKey")

    where
      deserialiseBase16' :: Text -> Aeson.Parser ByteString
      deserialiseBase16' t =
        case (convertFromBase Base16 . T.encodeUtf8 $ t) of
          Left err -> fail $ "Failed to deserialise Base16 bytestring from text: '" <> T.unpack t <> "', error was: " <> err
          Right x  -> pure x

      deserialiseSigDSIGN' :: ByteString -> Aeson.Parser (SigDSIGN Ed25519DSIGN)
      deserialiseSigDSIGN' t =
        case rawDeserialiseSigDSIGN t of
          Nothing -> fail $ "Failed to parse Ed25519DSIGN signature from '" <> BC.unpack t <> "'."
          Just x  -> pure x

      deserialiseVerKeyDSIGN' :: ByteString -> Aeson.Parser (VerKeyDSIGN Ed25519DSIGN)
      deserialiseVerKeyDSIGN' t =
        case rawDeserialiseVerKeyDSIGN t of
          Nothing -> fail $ "Failed to parse Ed25519DSIGN verification key from '" <> BC.unpack t <> "'."
          Just x  -> pure x

instance Aeson.ToJSON SequenceNumber where
  toJSON (SequenceNumber n) = Aeson.Number (fromInteger $ toInteger n)

instance Aeson.FromJSON SequenceNumber where
  parseJSON = Aeson.withScientific "SequenceNumber" $ \n ->
    case toBoundedInteger n of
      Nothing           -> fail "Sequence number must be an integer."
      Just (i :: Int64) -> case seqFromIntegral i of
        Nothing -> fail "Sequence number must be >= 0."
        Just s  -> pure s

$(deriveJSON Aeson.defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 3 } ''Owner)
$(deriveJSON Aeson.defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 2 } ''PreImage)
