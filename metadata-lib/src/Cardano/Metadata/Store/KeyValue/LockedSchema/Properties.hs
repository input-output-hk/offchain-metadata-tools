module Cardano.Metadata.Store.KeyValue.LockedSchema.Properties where

import Data.Map.Strict
import qualified Data.Map.Strict as M

data KeyValueStore k v = KeyValueStore (Map k v)

keys :: KeyValueStore k v -> [k]
keys =
