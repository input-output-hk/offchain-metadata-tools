{-# LANGUAGE ScopedTypeVariables #-}
-- | Convenience functions for manipulating metadata in it's raw form.
--
-- Note that the assurances provided by this module are very weak.
-- If the JSON value being modified doesn't conform to the general
-- form of metadata (i.e. a top-level object with keys corresponding
-- to property names), then you may get unexpected results. 
module Cardano.Metadata.View.JSON where

import Data.Text (Text)
import Control.Lens
import Data.Aeson.Lens
import Data.Scientific
import Data.Maybe (fromMaybe)
import Numeric.Natural (Natural)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM

import Cardano.Metadata.View.JSON.Lens (sequenceNumberL, propValueL, sequenceNumbersL)

-- | Set the value of a property. If the property is not present,
-- creates it. If Nothing, destroys existing value.
setPropertyValue :: Text -> Maybe Aeson.Value -> (Aeson.Value -> Aeson.Value)
setPropertyValue propName propVal v = v & propValueL propName .~ propVal

-- | Get the value of a property, if it exists.
getPropertyValue :: Text -> Aeson.Value -> Maybe Aeson.Value
getPropertyValue propName v = v ^? propValueL propName . _Just

-- | Set the value of a property's sequence number. If the property is
-- not present, creates it. If Nothing, destroys existing sequence
-- number.
setSequenceNumber :: Text -> Maybe Natural -> (Aeson.Value -> Aeson.Value)
setSequenceNumber propName mSeqNum = sequenceNumberL propName .~ (Aeson.Number . fromIntegral <$> mSeqNum)

-- | Get the sequence number of a property's sequence number, if it
-- exists.
getSequenceNumber :: Text -> Aeson.Value -> Maybe Natural
getSequenceNumber propName v =
  case toBoundedInteger =<< v ^? sequenceNumberL propName . _Just . _Number of
    Just (i :: Int) -> Just $ fromIntegral i
    Nothing         -> Nothing

-- | Create and/or modify the sequence number of all present
-- properties.
setSequenceNumbers :: Natural -> Aeson.Value -> Aeson.Value
setSequenceNumbers seqNum = sequenceNumbersL ?~ Aeson.Number (fromIntegral seqNum)
