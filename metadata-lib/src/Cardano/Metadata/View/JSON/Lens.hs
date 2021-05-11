{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Metadata.View.JSON.Lens where

import Data.Text (Text)
import Control.Lens
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens

sequenceNumberL :: Text -> Traversal' Aeson.Value (Maybe Aeson.Value)
sequenceNumberL propName = _Object . at propName . non (Aeson.Object mempty) . _Object . at "sequenceNumber"

propValueL :: Text -> Traversal' Aeson.Value (Maybe Aeson.Value)
propValueL propName = _Object . at propName . non (Aeson.Object mempty) . _Object . at "value"

sequenceNumbersL :: Traversal' Aeson.Value (Maybe Aeson.Value)
sequenceNumbersL = members . _Object . at "sequenceNumber"
