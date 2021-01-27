{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Cardano.Metadata.Server.Types where

import Data.Text (Text)
import GHC.Generics
import qualified Data.Text as T
import Data.Functor.Identity (Identity)
import Text.Read (Read(readPrec), readEither)
import qualified Text.Read as Read (lift)
import Text.ParserCombinators.ReadP (choice, string)
import Data.Aeson (ToJSON, FromJSON, (.:), (.:?))
import Control.Applicative (some)
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.TH
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HM
import Text.Casing
import qualified Data.List.NonEmpty as NE
import GHC.Show (showSpace)

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
  deriving (Eq, Show, Semigroup, Monoid)

-- | An entry in the metadata system.
data EntryF f
  = EntryF { enSubject     :: Subject
           -- ^ The metadata subject, the on-chain identifier
           , enOwner       :: f Owner
           -- ^ Public key and signature attesting to ownership of the metadata entry in this registry.
           , enName        :: f Name
           -- ^ A human-readable name for the metadata subject, suitable for use in an interface
           , enDescription :: f Description
           -- ^ A human-readable description for the metadata subject, suitable for use in an interface
           , enPreImage    :: f PreImage
           -- ^ A pair of a hash function identifier and a bytestring, such that the bytestring is the preimage of the metadata subject under that hash function
           }
  deriving (Generic)

instance (Show (f Owner), Show (f Name), Show (f Description), Show (f PreImage), Functor f) => Show (EntryF f) where
  -- show (EntryF a b c d e) = "(EntryF (" <> show a <> ") (" <> show b <> ") (" <> show c <> ") (" <> show d <> "))"
  showsPrec n (EntryF a b c d e)
      = showParen (n >= 11) $
             showString "EntryF "
           . showsPrec 11 a
           . showSpace
           . showsPrec 11 b
           . showSpace
           . showsPrec 11 c
           . showSpace
           . showsPrec 11 d
           . showSpace
           . showsPrec 11 e

instance (Eq (f Owner), Eq (f Name), Eq (f Description), Eq (f PreImage), Functor f) => Eq (EntryF f) where
  (EntryF a1 a2 a3 a4 a5) == (EntryF b1 b2 b3 b4 b5) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5

newtype Entry = Entry (EntryF Identity)
  deriving (Eq, Show)

newtype PartialEntry = PartialEntry (EntryF Maybe)
  deriving (Eq, Show)

instance ToJSON PartialEntry where
  toJSON (PartialEntry (EntryF subj owner name desc preImage)) =
    Aeson.Object . HM.fromList $
      [ ("subject", Aeson.toJSON subj) ]
      <> optionalKey "owner" owner
      <> optionalKey "name" name
      <> optionalKey "description" desc
      <> optionalKey "preImage" preImage

    where
      optionalKey :: ToJSON a => Text -> Maybe a -> [(Text, Aeson.Value)]
      optionalKey _ Nothing    = []
      optionalKey key (Just a) = [(key, Aeson.toJSON a)]

instance FromJSON PartialEntry where
  parseJSON = Aeson.withObject "PartialEntry" $ \obj -> fmap PartialEntry $
     EntryF
       <$> obj .: "subject"
       <*> obj .:? "owner"
       <*> obj .:? "name"
       <*> obj .:? "description"
       <*> obj .:? "preImage"

instance ToJSON Entry where
  toJSON (Entry (EntryF subj owner name desc preImage)) =
    Aeson.Object . HM.fromList $
      [ ("subject"     , Aeson.toJSON subj)
      , ("owner"       , Aeson.toJSON owner)
      , ("name"        , Aeson.toJSON name)
      , ("description" , Aeson.toJSON desc)
      , ("preImage"    , Aeson.toJSON preImage)
      ]

instance FromJSON Entry where
  parseJSON = Aeson.withObject "Entry" $ \obj -> fmap Entry $
    EntryF
     <$> obj .: "subject"
     <*> obj .: "owner"
     <*> obj .: "name"
     <*> obj .: "description"
     <*> obj .: "preImage"

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

$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 3 } ''Owner)
$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 2 } ''AnnotatedSignature)
$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 4 } ''Property)
$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 2 } ''PreImage)
$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 4 } ''BatchRequest)
$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 5 } ''BatchResponse)
