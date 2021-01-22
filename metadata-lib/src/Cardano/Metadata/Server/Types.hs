{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Metadata.Server.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (Read(readPrec), readEither)
import qualified Text.Read as Read (lift)
import Text.ParserCombinators.ReadP (choice, string)
import Data.Aeson (ToJSON, FromJSON, (.:))
import Control.Applicative (some)
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.TH
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HM
import Text.Casing 
import qualified Data.List.NonEmpty as NE

-- | More loosely-typed metadata property useful in looser-typed
-- contexts.
-- 
-- For example, when the user queries the metadata server for a
-- property, we might return a preimage, or a subject, or an owner, or
-- some other generic property. We need a type to represent this.
data AnyProperty = PropertyPreImage PreImage
                 | PropertyOwner    Owner
                 | PropertyGeneric  Text Property
  deriving (Eq, Show)

anyPropertyJSONKey :: AnyProperty -> Text
anyPropertyJSONKey (PropertyPreImage _)  = "preImage"
anyPropertyJSONKey (PropertyOwner _)     = "owner"
anyPropertyJSONKey (PropertyGeneric k _) = k

anyPropertyToJSONObject :: AnyProperty -> HM.HashMap Text Aeson.Value
anyPropertyToJSONObject (PropertyGeneric k p)         = HM.singleton k (Aeson.toJSON p)
anyPropertyToJSONObject p@(PropertyPreImage preImage) = HM.singleton (anyPropertyJSONKey p) (Aeson.toJSON preImage)
anyPropertyToJSONObject p@(PropertyOwner own)         = HM.singleton (anyPropertyJSONKey p) (Aeson.toJSON own)

instance ToJSON AnyProperty where
  toJSON = Aeson.Object . anyPropertyToJSONObject

instance FromJSON AnyProperty where
  parseJSON = Aeson.withObject "AnyProperty" $ \obj -> do
    let keys = HM.keys obj

    case keys of
      (k:[]) ->
        case HM.lookup k obj of
          Nothing  -> Aeson.parseFail $ "Object has no value for key '" <> show k <> "'."
          Just val ->
            case k of
              "preImage" -> PropertyPreImage     <$> Aeson.parseJSON val
              "owner"    -> PropertyOwner        <$> Aeson.parseJSON val
              name       -> PropertyGeneric name <$> Aeson.parseJSON val
      otherwise       -> Aeson.parseFail $ "Object should only have one key but instead has: " <> show keys

-- | More loosely-typed metadata entry useful in looser-typed contexts.
--
-- For example, when the user queries with a batch request, they may
-- ask for the "description" and "owner" properties. We can't return
-- them a 'Entry' because we only have the "description" and
-- "owner" properties, therefore, we need a type which allows us to
-- represent part of a 'Entry', hence 'PartialEntry'.
data PartialEntry
  = PartialEntry { peSubject    :: Subject
                 , peProperties :: HM.HashMap Text AnyProperty
                 }
  deriving (Eq, Show)

instance ToJSON PartialEntry where
  toJSON (PartialEntry subj props) = (Aeson.Object $ HM.fromList
    [("subject", Aeson.String subj)] <> fmap Aeson.toJSON props)

instance FromJSON PartialEntry where
  parseJSON = Aeson.withObject "PartialEntry" $ \obj ->
    PartialEntry
      <$> (obj .: "subject")
      <*> (fmap HM.fromList $ sequence (fmap
                    (\(k, v) -> (k,) <$> Aeson.parseJSON v)
                    (filter ((/= "subject") . fst) $ HM.toList obj)
                   ))

-- | Represents the content of a batch request to the metadata system.
--
-- For example, @BatchRequest ["a", "b"] ["preimage", "name"]@ would
-- represent a request for the "preimage" and "name" properties of both
-- subject "a" and "b".
data BatchRequest
  = BatchRequest { bReqSubjects   :: [Subject]
                 , bReqProperties :: [Text]
                 }
  deriving (Eq, Show)

-- | Represents the response of a batch request.
data BatchResponse
  = BatchResponse { bRespSubjects :: [PartialEntry] }
  deriving (Eq, Show)

-- | An entry in the metadata system.
data Entry
  = Entry { enSubject     :: Subject
          -- ^ The metadata subject, the on-chain identifier
          , enOwner       :: Owner
          -- ^ Public key and signature attesting to ownership of the metadata entry in this registry.
          , enName        :: Name
          -- ^ A human-readable name for the metadata subject, suitable for use in an interface
          , enDescription :: Description
          -- ^ A human-readable description for the metadata subject, suitable for use in an interface
          , enPreImage    :: PreImage
          -- ^ A pair of a hash function identifier and a bytestring, such that the bytestring is the preimage of the metadata subject under that hash function
          }
  deriving (Eq, Show)

-- | The metadata subject, the on-chain identifier
type Subject = Text

-- | Public key and signature attesting to ownership of the metadata
-- entry in this registry.
data Owner
  = Owner { ownSignature :: Text
          , ownPublicKey :: Text
          }
  deriving (Eq, Show)

-- | A human-readable name for the metadata subject, suitable for use in an interface
type Name = Property

-- | A human-readable description for the metadata subject, suitable for use in an interface
type Description = Property

-- | A pair of a hash function identifier and a bytestring, such that
-- the bytestring is the preimage of the metadata subject under that hash
-- function.
data PreImage
  = PreImage { piValue  :: Text
             , piHashFn :: HashFn
             }
  deriving (Eq, Show)

-- | A pair of the value, and a list of annotated signatures.
data Property
  = Property { propValue        :: Text
             , propAnSignatures :: [AnnotatedSignature]
             }
  deriving (Eq, Show)

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

instance ToJSON HashFn where
  toJSON = Aeson.String . T.pack .show

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

$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 2 } ''Entry)
$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 3 } ''Owner)
$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 2 } ''AnnotatedSignature)
$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 4 } ''Property)
$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 2 } ''PreImage)
$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 4 } ''BatchRequest)
$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 5 } ''BatchResponse)
