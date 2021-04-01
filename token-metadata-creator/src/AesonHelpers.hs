module AesonHelpers where

import Cardano.Prelude

import Control.Category
    ()

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import qualified Data.Text as T

noOtherFields
  :: Text
  -- ^ "type" of the object for error reporting purposes
  -> Aeson.Object
  -> [Text]
  -- ^ Fields we expect
  -> Aeson.Parser ()
noOtherFields tp o fs = Aeson.modifyFailure (\_ -> T.unpack tp <> " contained extraneous fields") $
  guard $ HSet.null $ HMap.keysSet o `HSet.difference` HSet.fromList fs
