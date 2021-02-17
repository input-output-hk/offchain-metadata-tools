{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Metadata.Server.Types where

import Data.Text (Text)
import Control.Monad ((>=>))
import Control.DeepSeq
    ( NFData )
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.URI
    ( URI, parseAbsoluteURI, uriScheme )
import Numeric.Natural
    ( Natural )
import Data.Functor (($>))
import Data.String (IsString, fromString)
import Data.ByteArray.Encoding
    ( Base (Base16, Base64), convertFromBase, convertToBase )
import Data.Monoid (First(First), getFirst)
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Functor.Identity (Identity(Identity))
import Text.Read (Read(readPrec), readEither)
import Web.HttpApiData (FromHttpApiData, parseUrlPiece, ToHttpApiData, toUrlPiece)
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

-- | Specification of a larger unit for an asset. For example, the "lovelace"
-- asset has the larger unit "ada" with 6 zeroes.
data AssetUnit = AssetUnit
    { name :: Text -- ^ Name of the larger asset.
    , decimals :: Natural  -- ^ Number of zeroes to add to base unit.
    } deriving (Generic, Show, Eq, Ord)

instance NFData AssetUnit

-- | Specify an asset logo as an image data payload
newtype AssetLogo = AssetLogo
    { unAssetLogo :: Encoded 'Base64
    } deriving (Eq, Ord, Generic, Show)

instance NFData AssetLogo

-- | The validated URL for the asset.
newtype AssetURL = AssetURL
    { unAssetURL :: URI
    } deriving (Eq, Ord, Generic, Show)

instance NFData AssetURL

validateMinLength :: Int -> Text -> Either String Text
validateMinLength n text
    | len >= n = Right text
    | otherwise = Left $ "Length must be at least " ++ show n ++ " characters, got " ++ show len
  where
    len = T.length text

validateMaxLength :: Int -> Text -> Either String Text
validateMaxLength n text
    | len <= n = Right text
    | otherwise = Left $ "Length must be no more than " ++ show n ++ " characters, got " ++ show len
  where
    len = T.length text

validateMetadataName :: Text -> Either String Text
validateMetadataName = validateMinLength 1 >=> validateMaxLength 50

validateMetadataAcronym :: Text -> Either String Text
validateMetadataAcronym = validateMinLength 2 >=> validateMaxLength 4

validateMetadataDescription :: Text -> Either String Text
validateMetadataDescription = validateMaxLength 500

validateMetadataURL :: Text -> Either String AssetURL
validateMetadataURL = fmap AssetURL .
    (validateMaxLength 250 >=> validateURI >=> validateHttps)
  where
      validateURI = maybe (Left "Not an absolute URI") Right
          . parseAbsoluteURI
          . T.unpack
      validateHttps u@(uriScheme -> scheme)
          | scheme == "https:" = Right u
          | otherwise = Left $ "Scheme must be https: but got " ++ scheme

validateMetadataUnit :: AssetUnit -> Either String AssetUnit
validateMetadataUnit assetUnit@AssetUnit{name, decimals} =
    (validateMinLength 1 name >>= validateMaxLength 30 >> validateDecimals decimals) $> assetUnit

validateDecimals :: Natural -> Either String Natural
validateDecimals n | n < 0  = Left $ "Number of decimals must be positive (> 0), got " <> show n
validateDecimals n | n > 19 = Left $ "Number of decimals must be less than 19, got " <> show n
validateDecimals n          = Right n

validateMetadataLogo :: AssetLogo -> Either String AssetLogo
validateMetadataLogo logo
    | len <= assetLogoMaxLength = Right logo
    | otherwise = Left $ "Length must be no more than " ++ show assetLogoMaxLength ++ " bytes, got " ++ show len
  where
    len = BS.length . raw . unAssetLogo $ logo

assetLogoMaxLength = 65536

resultToEither :: Aeson.Result a -> Either String a
resultToEither = \case
    Aeson.Success a -> Right a
    Aeson.Error e -> Left e

applyValidator :: (a -> Either String b) -> a -> Aeson.Parser b
applyValidator validate = either fail pure . validate

instance FromJSON AssetURL where
  parseJSON = Aeson.parseJSON >=> applyValidator validateMetadataURL

instance ToJSON AssetURL where
  toJSON (AssetURL uri) = Aeson.String $ T.pack $ show uri

instance FromJSON AssetLogo where
  parseJSON = (applyValidator validateMetadataLogo =<<) . fmap AssetLogo . Aeson.parseJSON

instance ToJSON AssetLogo where
  toJSON (AssetLogo bs) = Aeson.toJSON bs

instance FromJSON AssetUnit where
  parseJSON = (applyValidator validateMetadataUnit =<<) . (Aeson.withObject "AssetUnit" $ \o -> AssetUnit
      <$> o .: "name"
      <*> o .: "decimals")

instance ToJSON AssetUnit where
  toJSON (AssetUnit name decimals) = Aeson.Object $ HM.fromList $ [ ("name", Aeson.toJSON name)
                                                                  , ("decimals", Aeson.toJSON decimals)
                                                                  ]

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

  PropAcronym :: PropertyType Acronym
               -> GenericProperty Acronym
               -> Property Acronym

  PropAssetURL :: PropertyType AssetURL
               -> GenericProperty AssetURL
               -> Property AssetURL

  PropAssetUnit :: PropertyType AssetUnit
               -> GenericProperty AssetUnit
               -> Property AssetUnit

  PropAssetLogo :: PropertyType AssetLogo
               -> GenericProperty AssetLogo
               -> Property AssetLogo

data PropertyType t where
  PropTypeSubject     :: PropertyType Subject
  PropTypeOwner       :: PropertyType Owner
  PropTypeName        :: PropertyType Name
  PropTypeDescription :: PropertyType Description
  PropTypeAcronym     :: PropertyType Acronym
  PropTypeAssetURL    :: PropertyType AssetURL
  PropTypeAssetUnit   :: PropertyType AssetUnit
  PropTypeAssetLogo   :: PropertyType AssetLogo

type Acronym = Text

newtype Encoded (base :: Base) = Encoded
    { raw :: ByteString }
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


-- | Represents the content of a batch request to the metadata system.
--
-- For example, @BatchRequest ["a", "b"] ["preimage", "name"]@ would
-- represent a request for the "preimage" and "name" properties of both
-- subject "a" and "b".
data BatchRequest
  = BatchRequest { bReqSubjects   :: [Subject]
                 , bReqProperties :: Maybe [PropertyName]
                 }
  deriving (Eq, Show)

instance ToJSON BatchRequest where
  toJSON (BatchRequest subjs propNames) = Aeson.Object . HM.fromList $
    [ ("subjects", Aeson.toJSON subjs)
    , ("properties", Aeson.toJSON propNames)
    ]

instance FromJSON BatchRequest where
  parseJSON = Aeson.withObject "BatchRequest" $ \obj ->
    BatchRequest
      <$> obj .:  "subjects"
      <*> obj .:? "properties"

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
    entry   <- Entry
     <$> obj .: (propertyNameText PropTypeName)
     <*> obj .: (propertyNameText PropTypeDescription)
     <*> obj .:? (propertyNameText PropTypeOwner)
     <*> obj .:? (propertyNameText PropTypeAcronym)
     <*> obj .:? (propertyNameText PropTypeAssetURL)
     <*> obj .:? (propertyNameText PropTypeAssetLogo)
     <*> obj .:? (propertyNameText PropTypeAssetUnit)
    pure (Entry' subject entry)

instance FromJSON PartialEntry' where
  parseJSON = Aeson.withObject "PartialEntry'" $ \obj -> do
    subject      <- obj .: (propertyNameText PropTypeSubject)
    partialEntry <- fmap PartialEntry $ EntryF
      <$> (First <$> obj .:? propertyNameText PropTypeOwner)
      <*> (First <$> obj .:? propertyNameText PropTypeName)
      <*> (First <$> obj .:? propertyNameText PropTypeDescription)
      <*> (First <$> obj .:? propertyNameText PropTypeAcronym)
      <*> (First <$> obj .:? propertyNameText PropTypeAssetURL)
      <*> (First <$> obj .:? propertyNameText PropTypeAssetLogo)
      <*> (First <$> obj .:? propertyNameText PropTypeAssetUnit)
    pure (PartialEntry' subject partialEntry)

-- TODO ^ API Specific types

newtype PropertyName = PropertyName { getPropertyName :: Text }
  deriving (Eq, Show)

instance FromHttpApiData PropertyName where
  parseUrlPiece = Right . PropertyName

instance ToHttpApiData PropertyName where
  toUrlPiece = getPropertyName

instance IsString PropertyName where
  fromString = PropertyName . T.pack

instance ToJSON PropertyName where
  toJSON = Aeson.String . getPropertyName

instance FromJSON PropertyName where
  parseJSON = Aeson.withText "PropertyName" (pure . PropertyName)

propertyName :: PropertyType r -> PropertyName
propertyName PropTypeSubject     = PropertyName "subject"
propertyName PropTypeOwner       = PropertyName "owner"
propertyName PropTypeName        = PropertyName "name"
propertyName PropTypeDescription = PropertyName "description"
propertyName PropTypeAcronym     = PropertyName "acronym"
propertyName PropTypeAssetURL    = PropertyName "url"
propertyName PropTypeAssetLogo   = PropertyName "logo"
propertyName PropTypeAssetUnit   = PropertyName "unit"

propertyNameText :: PropertyType r -> Text
propertyNameText = getPropertyName . propertyName

nameToPropertyType :: (forall t. Maybe (PropertyType t) -> m) -> Text -> m
nameToPropertyType f "subject"     = f $ Just PropTypeSubject
nameToPropertyType f "owner"       = f $ Just PropTypeOwner
nameToPropertyType f "name"        = f $ Just PropTypeName
nameToPropertyType f "description" = f $ Just PropTypeDescription
nameToPropertyType f "acronym"     = f $ Just PropTypeAcronym
nameToPropertyType f "url"         = f $ Just PropTypeAssetURL
nameToPropertyType f "logo"        = f $ Just PropTypeAssetLogo
nameToPropertyType f "unit"        = f $ Just PropTypeAssetUnit
nameToPropertyType f _             = f Nothing

toJSONObject :: Property t -> HM.HashMap Text Aeson.Value
toJSONObject (PropSubject t subj)            = HM.singleton (propertyNameText t) (Aeson.toJSON subj)
toJSONObject (PropOwner t owner)             = HM.singleton (propertyNameText t) (Aeson.toJSON owner)
toJSONObject (PropName t name)               = HM.singleton (propertyNameText t) (Aeson.toJSON name)
toJSONObject (PropDescription t description) = HM.singleton (propertyNameText t) (Aeson.toJSON description)
toJSONObject (PropAcronym t acronym)         = HM.singleton (propertyNameText t) (Aeson.toJSON acronym)
toJSONObject (PropAssetURL t url)            = HM.singleton (propertyNameText t) (Aeson.toJSON url)
toJSONObject (PropAssetLogo t logo)          = HM.singleton (propertyNameText t) (Aeson.toJSON logo)
toJSONObject (PropAssetUnit t unit)          = HM.singleton (propertyNameText t) (Aeson.toJSON unit)

class HasProperties a where
  withProperties :: Monoid m => (forall t. Property t -> m) -> a -> m

instance HasProperties Entry where
  withProperties f (Entry name desc owner acronym url logo unit) =
    (maybe mempty (f . PropOwner PropTypeOwner) owner)
    <> f (PropName PropTypeName name)
    <> f (PropDescription PropTypeDescription desc)
    <> (maybe mempty (f . PropAcronym PropTypeAcronym) acronym)
    <> (maybe mempty (f . PropAssetURL PropTypeAssetURL) url)
    <> (maybe mempty (f . PropAssetLogo PropTypeAssetLogo) logo)
    <> (maybe mempty (f . PropAssetUnit PropTypeAssetUnit) unit)

instance ToJSON Entry where
  toJSON = Aeson.Object . withProperties toJSONObject

instance FromJSON Entry where
  parseJSON = Aeson.withObject "Entry" $ \obj -> Entry
     <$> obj .: (propertyNameText PropTypeName)
     <*> obj .: (propertyNameText PropTypeDescription)
     <*> obj .:? (propertyNameText PropTypeOwner)
     <*> obj .:? (propertyNameText PropTypeAcronym)
     <*> obj .:? (propertyNameText PropTypeAssetURL)
     <*> obj .:? (propertyNameText PropTypeAssetLogo)
     <*> obj .:? (propertyNameText PropTypeAssetUnit)

instance HasProperties PartialEntry where
  withProperties f (PartialEntry (EntryF (First mOwner) (First mName) (First mDesc) (First mAcronym) (First mUrl) (First mLogo) (First mUnit))) =
       (fromMaybe mempty $ fmap (f . PropOwner PropTypeOwner) mOwner)
    <> (fromMaybe mempty $ fmap (f . PropName PropTypeName) mName)
    <> (fromMaybe mempty $ fmap (f . PropDescription PropTypeDescription) mDesc)
    <> (fromMaybe mempty $ fmap (f . PropAcronym PropTypeAcronym) mAcronym)
    <> (fromMaybe mempty $ fmap (f . PropAssetURL PropTypeAssetURL) mUrl)
    <> (fromMaybe mempty $ fmap (f . PropAssetLogo PropTypeAssetLogo) mLogo)
    <> (fromMaybe mempty $ fmap (f . PropAssetUnit PropTypeAssetUnit) mUnit)

instance ToJSON PartialEntry where
  toJSON = Aeson.Object . withProperties toJSONObject

instance FromJSON PartialEntry where
  parseJSON = Aeson.withObject "PartialEntry" $ \obj -> fmap PartialEntry $
     EntryF
       <$> (First <$> obj .:? propertyNameText PropTypeOwner)
       <*> (First <$> obj .:? propertyNameText PropTypeName)
       <*> (First <$> obj .:? propertyNameText PropTypeDescription)
       <*> (First <$> obj .:? propertyNameText PropTypeAcronym)
       <*> (First <$> obj .:? propertyNameText PropTypeAssetURL)
       <*> (First <$> obj .:? propertyNameText PropTypeAssetLogo)
       <*> (First <$> obj .:? propertyNameText PropTypeAssetUnit)

-- | An entry in the metadata system.
data EntryF f
  = EntryF { enOwner       :: f Owner
           -- ^ Public key and signature attesting to ownership of the metadata entry in this registry.
           , enName        :: f Name
           -- ^ A human-readable name for the metadata subject, suitable for use in an interface
           , enDescription :: f Description
           -- ^ A human-readable description for the metadata subject, suitable for use in an interface
           , enAcronym     :: f (GenericProperty Acronym)
           , enURL         :: f (GenericProperty AssetURL)
           , enLogo        :: f (GenericProperty AssetLogo)
           , enUnit        :: f (GenericProperty AssetUnit)
           }
  deriving (Generic)

instance (Show (f Owner), Show (f Name), Show (f Description), Show (f (GenericProperty Acronym)), Show (f (GenericProperty AssetURL)), Show (f (GenericProperty AssetLogo)), Show (f (GenericProperty AssetUnit)), Functor f) => Show (EntryF f) where
  showsPrec n (EntryF a b c d e f g)
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
           . showSpace
           . showsPrec 11 f
           . showSpace
           . showsPrec 11 g

instance (Eq (f Owner), Eq (f Name), Eq (f Description), Eq (f (GenericProperty Acronym)), Eq (f (GenericProperty AssetURL)), Eq (f (GenericProperty AssetLogo)), Eq (f (GenericProperty AssetUnit)), Functor f) => Eq (EntryF f) where
  (EntryF a1 a2 a3 a4 a5 a6 a7) == (EntryF b1 b2 b3 b4 b5 b6 b7) = a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5 && a6 == b6 && a7 == b7

instance (Semigroup (f Owner), Semigroup (f Name), Semigroup (f Description), Semigroup (f (GenericProperty Acronym)), Semigroup (f (GenericProperty AssetURL)), Semigroup (f (GenericProperty AssetLogo)), Semigroup (f (GenericProperty AssetUnit))) => Semigroup (EntryF f) where
  (EntryF a1 a2 a3 a4 a5 a6 a7) <> (EntryF b1 b2 b3 b4 b5 b6 b7) = EntryF (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6) (a7 <> b7)

instance (Monoid (f Owner), Monoid (f Name), Monoid (f Description), Monoid (f (GenericProperty Acronym)), Monoid (f (GenericProperty AssetURL)), Monoid (f (GenericProperty AssetLogo)), Monoid (f (GenericProperty AssetUnit))) => Monoid (EntryF f) where
  mempty = EntryF mempty mempty mempty mempty mempty mempty mempty

data Entry
  = Entry { entryName        :: Name
          , entryDescription :: Description
          , entryOwner       :: Maybe Owner
          , entryAcronym     :: Maybe (GenericProperty Acronym)
          , entryURL         :: Maybe (GenericProperty AssetURL)
          , entryLogo        :: Maybe (GenericProperty AssetLogo)
          , entryUnit        :: Maybe (GenericProperty AssetUnit)
          }
  deriving (Eq, Show)

newtype PartialEntry = PartialEntry (EntryF First)
  deriving (Eq, Show)

instance Monoid PartialEntry where
  mempty = PartialEntry $ EntryF mempty mempty mempty mempty mempty mempty mempty

instance Semigroup PartialEntry where
  (PartialEntry (EntryF a1 a2 a3 a4 a5 a6 a7)) <> (PartialEntry (EntryF b1 b2 b3 b4 b5 b6 b7))
    = PartialEntry $ EntryF (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6) (a7 <> b7)

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
type Name = GenericProperty Text

-- | A human-readable description for the metadata subject, suitable for use in an interface
type Description = GenericProperty Text

-- | A pair of a hash function identifier and a bytestring, such that
-- the bytestring is the preimage of the metadata subject under that hash
-- function.
data PreImage
  = PreImage { piValue  :: Text
             , piHashFn :: HashFn
             }
  deriving (Eq, Show)

-- | A pair of the value, and a list of annotated signatures.
data GenericProperty value
  = GenericProperty { propValue        :: value
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

instance ToJSON value => ToJSON (GenericProperty value) where
  toJSON (GenericProperty value sigs) = Aeson.Object $ HM.fromList $
    [ ("value", Aeson.toJSON value)
    ] <> (case sigs of
            [] -> mempty
            xs -> [("anSignatures", Aeson.toJSON xs)]
         )

instance FromJSON value => FromJSON (GenericProperty value) where
  parseJSON = Aeson.withObject "GenericProperty" $ \obj ->
    GenericProperty 
      <$> obj .: "value"
      <*> (fromMaybe [] <$> obj .:? "anSignatures")

-- From cardano-wallet:
-- JSON deriving
$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 3 } ''Owner)
$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 2 } ''AnnotatedSignature)
$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 2 } ''PreImage)
$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 5 } ''BatchResponse)
