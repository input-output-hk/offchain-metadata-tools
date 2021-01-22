{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Metadata.Server.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (Read(readPrec))
import qualified Text.Read as Read (lift)
import Text.ParserCombinators.ReadP (choice, string)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH
import qualified Data.HashMap.Strict as HM
import Text.Casing 

-- | More loosely-typed metadata property useful in looser-typed
-- contexts.
-- 
-- For example, when the user queries the metadata server for a
-- property, we might return a preimage, or a subject, or an owner, or
-- some other generic property. We need a type to represent this.
data AnyProperty = PropertyPreImage PreImage
                 | PropertySubject  Subject
                 | PropertyOwner    Owner
                 | PropertyGeneric  Text Property
  deriving (Eq, Show)

anyPropertyJSONKey :: AnyProperty -> Text
anyPropertyJSONKey (PropertyPreImage _)  = "preImage"
anyPropertyJSONKey (PropertySubject _)   = "subject"
anyPropertyJSONKey (PropertyOwner _)     = "owner"
anyPropertyJSONKey (PropertyGeneric k _) = k

instance ToJSON AnyProperty where
  toJSON (PropertyGeneric _ p) = Aeson.toJSON p
  toJSON p                     = Aeson.toJSON p

-- | More loosely-typed metadata entry useful in looser-typed contexts.
--
-- For example, when the user queries with a batch request, they may
-- ask for the "description" and "owner" properties. We can't return
-- them a 'Entry' because we only have the "description" and
-- "owner" properties, therefore, we need a type which allows us to
-- represent part of a 'Entry', hence 'PartialEntry'.
data PartialEntry
  = PartialEntry { peSubject    :: Subject
                 , peProperties :: [AnyProperty]
                 }
  deriving (Eq, Show)

instance ToJSON PartialEntry where
  toJSON (PartialEntry subj props) = (Aeson.Object $ HM.fromList
    [("subject", Aeson.String subj)] <> foldMap toObj props)

    where
      toObj :: AnyProperty -> HM.HashMap Text Aeson.Value
      toObj p = HM.singleton (anyPropertyJSONKey p) $ Aeson.toJSON p

-- | Represents the content of a batch request to the metadata system.
--
-- For example, @BatchRequest ["a", "b"] ["preimage", "name"]@ would
-- represent a request for the "preimage" and "name" properties of both
-- subject "a" and "b".
data BatchRequest
  = BatchRequest { bReqSubjects      :: [Subject]
                 , bReqPropertyNames :: [Text]
                 }
  deriving (Eq, Show)

-- | Represents the response of a batch request.
data BatchResponse
  = BatchResponse { bRespSubjects :: [PartialEntry] }
  deriving (Eq, Show)

instance ToJSON BatchResponse where
  toJSON (BatchResponse subjects) = Aeson.Object $ HM.fromList [("subjects", Aeson.toJSON subjects)]

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
  = Owner { ownPublicKey :: Text
          , ownSignature :: Text
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

instance ToJSON PreImage where
  toJSON (PreImage val hashFn) = Aeson.Object $ HM.fromList
    [ ("value", Aeson.String val)
    , ("hashFn", Aeson.String $ T.pack . show $ hashFn)]

-- | A pair of the value, and a list of annotated signatures.
data Property
  = Property { propValue        :: Text
             , propAnSignatures :: [AnnotatedSignature]
             }
  deriving (Eq, Show)

instance ToJSON Property where
  toJSON (Property val anSigs) = Aeson.Object $ HM.fromList
    [ ("value", Aeson.toJSON val)
    , ("anSignatures", Aeson.toJSON anSigs)
    ]

-- | A pair of a public key, and a signature of the metadata entry by
-- that public key.
data AnnotatedSignature =
  AnnotatedSignature { asPublicKey :: Text
                     , asSignature :: Text
                     }
  deriving (Eq, Show)

instance ToJSON AnnotatedSignature where
  toJSON (AnnotatedSignature pubKey sig) = Aeson.Object $ HM.fromList $
    [ ("signature", Aeson.String sig)
    , ("publicKey", Aeson.String pubKey)
    ]

-- | Hash functions supported by 'PreImage'.
data HashFn = Blake2b256
            | Blake2b224
            | SHA256
  deriving (Eq)

instance Show HashFn where
  show Blake2b256 = "blake2b-256"
  show Blake2b224 = "blake2b-224"
  show SHA256     = "sha256"

instance Read HashFn where
  readPrec = Read.lift $ choice [ Blake2b256 <$ string "blake2b-256"
                                , Blake2b224 <$ string "blake2b-224"
                                , SHA256     <$ string "sha256"
                                ]

$(deriveJSON defaultOptions{Aeson.fieldLabelModifier = drop 2, Aeson.constructorTagModifier = toCamel . fromHumps } ''Entry)
$(deriveJSON defaultOptions{Aeson.fieldLabelModifier = drop 3, Aeson.constructorTagModifier = toCamel . fromHumps } ''Owner)
$(deriveJSON defaultOptions{Aeson.fieldLabelModifier = drop 2, Aeson.constructorTagModifier = toCamel . fromHumps } ''Owner)
