{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

module Cardano.Metadata.Types.Wallet where

import           Control.DeepSeq               (NFData)
import           Control.Monad                 ((>=>))
import           Data.Aeson                    (FromJSON, ToJSON, (.:), (.:?))
import qualified Data.Aeson.Types              as Aeson
import qualified Data.Bifunctor                as Bifunctor
import           Data.ByteArray.Encoding       (Base (Base16, Base64), convertToBase)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8 as B8
import           Data.Functor                  (($>))
import qualified Data.HashMap.Strict           as HM
import           Data.String                   (IsString)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding                     as T
import           GHC.Generics                  (Generic)
import           Network.URI                   (URI, parseAbsoluteURI,
                                                uriScheme)
import           Numeric.Natural               (Natural)
import           Quiet                         (Quiet (Quiet))
import qualified Cardano.Metadata.Types as Submitter
import qualified Cardano.Api as Cardano

import           Cardano.Metadata.Types.Common (Description, Encoded, Name,
                                                Property(Property), PropertyName,
                                                Subject (Subject),
                                                propertyValue, rawEncoded,
                                                unPropertyName, unSubject)
import qualified Cardano.Metadata.Types.Weakly as Weakly

data Metadata
  = Metadata { metaSubject     :: Subject
             , metaPolicy      :: Policy
             , metaName        :: Name
             , metaDescription :: Description
             , metaUnit        :: Maybe (Property AssetUnit)
             , metaLogo        :: Maybe (Property AssetLogo)
             , metaURL         :: Maybe (Property AssetURL)
             , metaTicker      :: Maybe (Property Ticker)
             , metaOther       :: HM.HashMap PropertyName Weakly.Property
             }
  deriving (Show, Eq)

-- | Specification of a larger unit for an asset. For example, the "lovelace"
-- asset has the larger unit "ada" with 6 zeroes.
data AssetUnit = AssetUnit
    { name     :: Text -- ^ Name of the larger asset.
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

newtype Ticker = Ticker { unTicker :: Text }
  deriving (Generic, Eq, Ord, ToJSON)
  deriving newtype (IsString)
  deriving (Show) via (Quiet Ticker)

instance FromJSON Ticker where
  parseJSON = Aeson.withText "Ticker" $ \t ->
    Ticker <$> applyValidator validateMetadataTicker t

data Policy = Policy Submitter.Policy

verifyPolicy :: Policy -> Subject -> Either Text ()
verifyPolicy (Policy submitterPolicy) (Subject subj) =
  Submitter.verifyPolicy submitterPolicy (Submitter.Subject subj)

policyId :: Policy -> Text
policyId (Policy policy) = T.pack . B8.unpack . Cardano.serialiseToRawBytesHex . Submitter.hashPolicy $ policy

mkPolicy :: Cardano.ScriptInEra Cardano.MaryEra -> Policy
mkPolicy script =
  let
    scriptText = T.decodeUtf8 $ convertToBase Base16 $ Cardano.serialiseToCBOR script
  in
    Policy (Submitter.Policy scriptText script)

fromWeaklyTypedMetadata :: Weakly.Metadata -> Aeson.Parser Metadata
fromWeaklyTypedMetadata (Weakly.Metadata subj props) =
  let
    obj :: Aeson.Object
    obj = HM.fromList $ fmap (Bifunctor.bimap unPropertyName Aeson.toJSON) $ HM.toList $ props

    rest :: HM.HashMap PropertyName Weakly.Property
    rest = foldr HM.delete props ["name", "description", "unit", "logo", "url", "ticker", "policy"]
  in do
    subject <- (applyValidator validateMetadataSubject subj)
    policy  <- obj .: "policy"
    applyValidator (Bifunctor.first T.unpack . verifyPolicy policy) subject

    Metadata subject policy
      <$> (validateProp validateMetadataName =<< obj .: "name")
      <*> (validateProp validateMetadataDescription =<< obj .: "description")
      <*> obj .:? "unit"
      <*> obj .:? "logo"
      <*> obj .:? "url"
      <*> obj .:? "ticker"
      <*> pure rest

  where
    validateProp :: (a -> Either String a) -> Property a -> Aeson.Parser (Property a)
    validateProp validator prop = do
      _ <- applyValidator validator (propertyValue prop)
      pure prop

toWeaklyTypedMetadata :: Metadata -> Weakly.Metadata
toWeaklyTypedMetadata (Metadata subj policy name desc unit logo url ticker rest) =
  Weakly.Metadata subj $
    HM.fromList $
      [ ("name"       , Weakly.toWeaklyTypedProperty name)
      , ("description", Weakly.toWeaklyTypedProperty desc)
      , ("policy", Property (Aeson.toJSON policy) Nothing)
      ] <> optionalProperty "unit" unit
        <> optionalProperty "logo" logo
        <> optionalProperty "url" url
        <> optionalProperty "ticker" ticker
        <> HM.toList rest
  where
    optionalProperty :: ToJSON a => PropertyName -> Maybe (Property a) -> [(PropertyName, Property Aeson.Value)]
    optionalProperty _        Nothing      = []
    optionalProperty propName (Just prop)  = [(propName, Weakly.toWeaklyTypedProperty prop)]

-- Validation (TODO move this)

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

validateMetadataSubject :: Subject -> Either String Subject
validateMetadataSubject = fmap Subject . (validateMinLength 1 >=> validateMaxLength 256) . unSubject

validateMetadataName :: Text -> Either String Text
validateMetadataName = validateMinLength 1 >=> validateMaxLength 50

validateMetadataTicker :: Text -> Either String Text
validateMetadataTicker = validateMinLength 2 >=> validateMaxLength 4

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
validateDecimals n | n < 1  = Left $ "Number of decimals must be greater than 1, got " <> show n
validateDecimals n | n > 19 = Left $ "Number of decimals must be less than 19, got " <> show n
validateDecimals n          = Right n

validateMetadataLogo :: AssetLogo -> Either String AssetLogo
validateMetadataLogo logo
    | len <= assetLogoMaxLength = Right logo
    | otherwise = Left $ "Length must be no more than " ++ show assetLogoMaxLength ++ " bytes, got " ++ show len
  where
    len = BS.length . rawEncoded . unAssetLogo $ logo

assetLogoMaxLength :: Int
assetLogoMaxLength = 65536

resultToEither :: Aeson.Result a -> Either String a
resultToEither = \case
    Aeson.Success a -> Right a
    Aeson.Error e -> Left e

applyValidator :: (a -> Either String b) -> a -> Aeson.Parser b
applyValidator validate = either fail pure . validate


-- Instances

instance ToJSON Metadata where
  toJSON = Aeson.toJSON . toWeaklyTypedMetadata

instance FromJSON Metadata where
  parseJSON val = fromWeaklyTypedMetadata =<< Aeson.parseJSON val

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

instance ToJSON Policy where
  toJSON (Policy x) = Submitter.wellKnownToJSON x

instance FromJSON Policy where
  parseJSON = fmap Policy . Submitter.parseWellKnown

instance Show Policy where
  show (Policy (Submitter.Policy raw _script)) = T.unpack raw

instance Eq Policy where
  (Policy (Submitter.Policy raw1 _script1)) == (Policy (Submitter.Policy raw2 _script2)) = raw1 == raw2
