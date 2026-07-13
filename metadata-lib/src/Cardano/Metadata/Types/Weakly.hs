{-# LANGUAGE OverloadedStrings #-}

module Cardano.Metadata.Types.Weakly where

import Data.Aeson ( FromJSON, ToJSON, (.:) )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as HM

import Cardano.Metadata.Types.Common
    ( PropertyName, Subject, fromPropertyNameList, toPropertyNameList )


data Metadata
  = Metadata { metaSubject    :: Subject
             , metaProperties :: HM.HashMap PropertyName Aeson.Value
             }
  deriving (Eq, Show)

getMetadataProperty :: PropertyName -> Metadata -> Maybe Aeson.Value
getMetadataProperty propertyName (Metadata _ props) = HM.lookup propertyName props

-- JSON instances

instance ToJSON Metadata where
  toJSON (Metadata subject properties) = Aeson.Object $ KM.fromList $
    [ ("subject", Aeson.toJSON subject)
    ]
    <> ((\(k, v) -> (Key.fromText k, Aeson.toJSON v)) <$> (fromPropertyNameList $ HM.toList properties))

instance FromJSON Metadata where
  parseJSON = Aeson.withObject "Weakly-typed Metadata" $ \obj ->
    Metadata
    <$> obj .: "subject"
    <*> (traverse Aeson.parseJSON $ HM.fromList $ toPropertyNameList $ fmap (\(k, v) -> (Key.toText k, v)) $ KM.toList $ KM.delete "subject" obj)
