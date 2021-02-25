{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cardano.Metadata.Types.Common where

import           Control.DeepSeq              (NFData)
import           Data.Aeson                   (FromJSON, FromJSONKey, ToJSON,
                                               ToJSONKey, (.:))
import           Data.Aeson.TH                (deriveJSON)
import qualified Data.Aeson.Types             as Aeson
import           Data.ByteArray.Encoding      (Base (Base16, Base64),
                                               convertFromBase, convertToBase)
import           Data.ByteString              (ByteString)
import           Data.Hashable                (Hashable)
import qualified Data.HashMap.Strict          as HM
import           Data.String                  (IsString)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           GHC.Generics                 (Generic)
import           Quiet                        (Quiet (Quiet))
import           Text.Casing                  (fromHumps, toCamel)
import           Text.ParserCombinators.ReadP (choice, string)
import           Text.Read                    (readEither, readPrec)
import qualified Text.Read                    as Read (lift)
import           Web.HttpApiData              (FromHttpApiData, ToHttpApiData)

-- | The metadata subject, the on-chain identifier
newtype Subject = Subject { unSubject :: Text }
  deriving (Generic, Eq, Ord, FromHttpApiData, ToJSONKey, FromJSONKey, ToJSON, FromJSON, Hashable)
  deriving newtype (IsString)
  deriving (Show) via (Quiet Subject)

newtype PropertyName = PropertyName { unPropertyName :: Text }
  deriving (Generic, Eq, Ord, FromHttpApiData, ToJSONKey, FromJSONKey, ToJSON, FromJSON, Hashable)
  deriving newtype (IsString)
  deriving (Show) via (Quiet PropertyName)

data Property value
  = Property { propertyValue        :: value
             , propertyAnSignatures :: [AnnotatedSignature]
             }
  deriving (Eq, Show)

-- | A human-readable name for the metadata subject, suitable for use in an interface
type Name        = Property Text

-- | A human-readable description for the metadata subject, suitable for use in an interface
type Description = Property Text

-- | A pair of a public key, and a signature of the metadata entry by
-- that public key.
data AnnotatedSignature =
  AnnotatedSignature { asSignature :: Text
                     , asPublicKey :: Text
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

-- Instances

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

instance ToJSON value => ToJSON (Property value) where
  toJSON (Property value sigs) = Aeson.Object $ HM.fromList $
    [ ("value", Aeson.toJSON value)
    , ("anSignatures", Aeson.toJSON sigs)
    ]

instance FromJSON value => FromJSON (Property value) where
  parseJSON = Aeson.withObject "Weakly-typed Property" $ \obj ->
    Property
      <$> obj .: "value"
      <*> obj .: "anSignatures"

$(deriveJSON Aeson.defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 2 } ''AnnotatedSignature)
$(deriveJSON Aeson.defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 3 } ''Owner)
$(deriveJSON Aeson.defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 2 } ''PreImage)
