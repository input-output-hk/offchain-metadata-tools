{-# LANGUAGE OverloadedStrings #-}

module Cardano.Metadata.Types.Weakly where

import Data.Aeson
    ( FromJSON, ToJSON, (.:) )
import qualified Data.Aeson as Aeson
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
  toJSON (Metadata subject properties) = Aeson.Object $ HM.fromList $
    [ ("subject", Aeson.toJSON subject)
    ]
    <> (fmap (Aeson.toJSON) <$> (fromPropertyNameList $ HM.toList properties))

instance FromJSON Metadata where
  parseJSON = Aeson.withObject "Weakly-typed Metadata" $ \obj ->
    Metadata
    <$> obj .: "subject"
    <*> (traverse Aeson.parseJSON $ HM.fromList $ toPropertyNameList $ HM.toList $ foldr HM.delete obj ["subject"])
