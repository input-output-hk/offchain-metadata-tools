{-# LANGUAGE OverloadedStrings #-}

module Cardano.Metadata.Types.Weakly where

import           Data.Aeson                    (FromJSON, ToJSON, (.:))
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Types              as Aeson
import qualified Data.HashMap.Strict           as HM
import           Data.Text                     (Text)

import           Cardano.Metadata.Types.Common (PropertyName (PropertyName),
                                                Subject, unPropertyName)
import qualified Cardano.Metadata.Types.Common as Strongly (Property (Property))

type Property = Strongly.Property Aeson.Value

toWeaklyTypedProperty :: ToJSON value => Strongly.Property value -> Property
toWeaklyTypedProperty (Strongly.Property value signatures) = (Strongly.Property (Aeson.toJSON value) signatures)

fromWeaklyTypedProperty :: FromJSON value => Property -> Aeson.Parser (Strongly.Property value)
fromWeaklyTypedProperty (Strongly.Property value signatures) = Strongly.Property <$> Aeson.parseJSON value <*> pure signatures

data Metadata
  = Metadata { metaSubject    :: Subject
             , metaProperties :: HM.HashMap PropertyName Property
             }
  deriving (Eq, Show)

toPropertyNameList :: [(Text, a)] -> [(PropertyName, a)]
toPropertyNameList = fmap (\(k, v) -> (PropertyName k, v))

fromPropertyNameList :: [(PropertyName, a)] -> [(Text, a)]
fromPropertyNameList = fmap (\(k, v) -> (unPropertyName k, v))

getMetadataProperty :: PropertyName -> Metadata -> Maybe Property
getMetadataProperty propertyName (Metadata _ props) = HM.lookup propertyName props

-- JSON instances

instance ToJSON Metadata where
  toJSON (Metadata subject properties) = Aeson.Object $ HM.fromList $
    [ ("subject", Aeson.toJSON subject)
    ]
    <> (fmap (Aeson.toJSON) <$> (fromPropertyNameList $ HM.toList properties))

instance FromJSON Metadata where
  parseJSON = Aeson.withObject "Weakly-typed Metadata" $ \obj ->
    Metadata
    <$> obj .: "subject"
    <*> (traverse Aeson.parseJSON $ HM.fromList $ toPropertyNameList $ HM.toList $ foldr HM.delete obj ["subject"])
