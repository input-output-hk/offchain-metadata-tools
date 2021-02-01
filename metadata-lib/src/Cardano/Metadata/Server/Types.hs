{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Metadata.Server.Types where

import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.Monoid (First(First), getFirst)
import GHC.Generics
import qualified Data.Text as T
import Data.Functor.Identity (Identity(Identity))
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
  = BatchResponse { bRespSubjects :: [PartialEntry'] }
  deriving (Eq, Show)

instance Semigroup BatchResponse where
  (BatchResponse xs) <> (BatchResponse ys) = BatchResponse $ xs <> ys

instance Monoid BatchResponse where
  mempty = BatchResponse mempty

data PartialEntry' = PartialEntry' { _peSubject      :: Subject
                                   , _pePartialEntry :: PartialEntry
                                   }
  deriving (Eq, Show)

data Entry' = Entry' { _eSubject :: Subject
                     , _eEntry   :: Entry
                     }
  deriving (Eq, Show)

instance HasProperties Entry' where
  withProperties f (Entry' subject entry) =
    f (PropSubject PropTypeSubject subject)
    <> withProperties f entry

instance HasProperties PartialEntry' where
  withProperties f (PartialEntry' subject partialEntry) =
    f (PropSubject PropTypeSubject subject)
    <> withProperties f partialEntry

instance ToJSON Entry' where
  toJSON = Aeson.Object . withProperties toJSONObject

instance ToJSON PartialEntry' where
  toJSON = Aeson.Object . withProperties toJSONObject

instance FromJSON Entry' where
  parseJSON = Aeson.withObject "Entry'" $ \obj -> do
    subject <- obj .: (propertyNameText PropTypeSubject)
    entry   <- fmap Entry $ EntryF
     <$> obj .: (propertyNameText PropTypeOwner)
     <*> obj .: (propertyNameText PropTypeName)
     <*> obj .: (propertyNameText PropTypeDescription)
     <*> obj .: (propertyNameText PropTypePreImage)
    pure (Entry' subject entry)

instance FromJSON PartialEntry' where
  parseJSON = Aeson.withObject "PartialEntry'" $ \obj -> do
    subject      <- obj .: (propertyNameText PropTypeSubject)
    partialEntry <- fmap PartialEntry $ EntryF
      <$> (First <$> obj .:? propertyNameText PropTypeOwner)
      <*> (First <$> obj .:? propertyNameText PropTypeName)
      <*> (First <$> obj .:? propertyNameText PropTypeDescription)
      <*> (First <$> obj .:? propertyNameText PropTypePreImage)
    pure (PartialEntry' subject partialEntry)

-- TODO ^ API Specific types

data Property t where
  PropSubject :: PropertyType Subject
              -> Subject
              -> Property Subject

  PropOwner :: PropertyType Owner
            -> Owner
            -> Property Owner

  PropName :: PropertyType Name
           -> Name
           -> Property Name

  PropDescription :: PropertyType Description
                  -> Description
                  -> Property Description

  PropPreImage :: PropertyType PreImage
               -> PreImage
               -> Property PreImage

data PropertyType t where
  PropTypeSubject     :: PropertyType Subject
  PropTypeOwner       :: PropertyType Owner
  PropTypeName        :: PropertyType Name
  PropTypeDescription :: PropertyType Description
  PropTypePreImage    :: PropertyType PreImage

newtype PropertyName = PropertyName { getPropertyName :: Text }
  deriving (Eq, Show)

propertyName :: PropertyType r -> PropertyName
propertyName PropTypeSubject = PropertyName "subject"
propertyName PropTypeOwner   = PropertyName "owner"

propertyNameText :: PropertyType r -> Text
propertyNameText = getPropertyName . propertyName

nameToPropertyType :: (forall t. Maybe (PropertyType t) -> m) -> Text -> m
nameToPropertyType f "subject" = f $ Just PropTypeSubject
nameToPropertyType f "owner"   = f $ Just PropTypeOwner
nameToPropertyType f _         = f Nothing

toJSONObject :: Property t -> HM.HashMap Text Aeson.Value
toJSONObject (PropSubject t subj) = HM.singleton (propertyNameText t) (Aeson.toJSON subj)
toJSONObject (PropOwner t owner)  = HM.singleton (propertyNameText t) (Aeson.toJSON owner)

class HasProperties a where
  withProperties :: Monoid m => (forall t. Property t -> m) -> a -> m

instance HasProperties Entry where
  withProperties f (Entry (EntryF (Identity owner) (Identity name) (Identity desc) (Identity preImage))) =
    f (PropOwner PropTypeOwner owner)
    <> f (PropName PropTypeName name)
    <> f (PropDescription PropTypeDescription desc)
    <> f (PropPreImage PropTypePreImage preImage)

instance ToJSON Entry where
  toJSON = Aeson.Object . withProperties toJSONObject

instance FromJSON Entry where
  parseJSON = Aeson.withObject "Entry" $ \obj -> fmap Entry $
    EntryF
     <$> obj .: (propertyNameText PropTypeOwner)
     <*> obj .: (propertyNameText PropTypeName)
     <*> obj .: (propertyNameText PropTypeDescription)
     <*> obj .: (propertyNameText PropTypePreImage)

instance HasProperties PartialEntry where
  withProperties f (PartialEntry (EntryF (First mOwner) (First mName) (First mDesc) (First mPreImage))) =
       (fromMaybe mempty $ fmap (f . PropOwner PropTypeOwner) mOwner)
    <> (fromMaybe mempty $ fmap (f . PropName PropTypeName) mName)
    <> (fromMaybe mempty $ fmap (f . PropDescription PropTypeDescription) mDesc)
    <> (fromMaybe mempty $ fmap (f . PropPreImage PropTypePreImage) mPreImage)

instance ToJSON PartialEntry where
  toJSON = Aeson.Object . withProperties toJSONObject

instance FromJSON PartialEntry where
  parseJSON = Aeson.withObject "PartialEntry" $ \obj -> fmap PartialEntry $
     EntryF
       <$> (First <$> obj .:? propertyNameText PropTypeOwner)
       <*> (First <$> obj .:? propertyNameText PropTypeName)
       <*> (First <$> obj .:? propertyNameText PropTypeDescription)
       <*> (First <$> obj .:? propertyNameText PropTypePreImage)

-- | An entry in the metadata system.
data EntryF f
  = EntryF { enOwner       :: f Owner
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
  showsPrec n (EntryF a b c d)
      = showParen (n >= 11) $
             showString "EntryF "
           . showsPrec 11 a
           . showSpace
           . showsPrec 11 b
           . showSpace
           . showsPrec 11 c
           . showSpace
           . showsPrec 11 d

instance (Eq (f Owner), Eq (f Name), Eq (f Description), Eq (f PreImage), Functor f) => Eq (EntryF f) where
  (EntryF a1 a2 a3 a4) == (EntryF b1 b2 b3 b4) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4

newtype Entry = Entry (EntryF Identity)
  deriving (Eq, Show)

newtype PartialEntry = PartialEntry (EntryF First)
  deriving (Eq, Show)

availablePropertyNames :: [Text]
availablePropertyNames = 
  [ "subject"
  ,  "owner"
  ,  "name"
  ,  "description"
  ,  "preImage"
  ]

instance Monoid PartialEntry where
  mempty = PartialEntry $ EntryF mempty mempty mempty mempty

instance Semigroup PartialEntry where
  (PartialEntry (EntryF a1 a2 a3 a4)) <> (PartialEntry (EntryF b1 b2 b3 b4))
    = PartialEntry $ EntryF (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

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
type Name = GenericProperty

-- | A human-readable description for the metadata subject, suitable for use in an interface
type Description = GenericProperty

-- | A pair of a hash function identifier and a bytestring, such that
-- the bytestring is the preimage of the metadata subject under that hash
-- function.
data PreImage
  = PreImage { piValue  :: Text
             , piHashFn :: HashFn
             }
  deriving (Eq, Show)

-- | A pair of the value, and a list of annotated signatures.
data GenericProperty
  = GenericProperty { propValue        :: Text
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
$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 4 } ''GenericProperty)
$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 2 } ''PreImage)
$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 4 } ''BatchRequest)
$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 5 } ''BatchResponse)
