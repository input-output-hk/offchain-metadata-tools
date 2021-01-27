{-# LANGUAGE RankNTypes #-}
module Cardano.Metadata.Store.KeyValue.Map where

import Data.Map.Strict (Map)
import Control.Concurrent.MVar
import qualified Data.Map.Strict as Map

newtype KeyValue k v = KeyValue (MVar (Map k v))

init :: Map k v -> IO (KeyValue k v)
init state = do
  mVar <- newMVar state
  pure (KeyValue mVar)

read :: Ord k => k -> KeyValue k v -> IO (Maybe v)
read k (KeyValue mVar) = do
  m <- readMVar mVar
  pure $ Map.lookup k m

write :: Ord k => k -> v -> KeyValue k v -> IO (KeyValue k v)
write k v = modifyKeyValue (Map.insert k v)

delete :: Ord k => k -> KeyValue k v -> IO (KeyValue k v)
delete k = modifyKeyValue (Map.delete k)

toList :: KeyValue k v -> IO [(k, v)]
toList (KeyValue mVar) = do
  m <- readMVar mVar
  pure $ Map.toList m

-- update :: k -> (v -> v) -> KeyValue k v -> IO (KeyValue k v)

modifyKeyValue :: (Map k v -> Map k v) -> KeyValue k v -> IO (KeyValue k v)
modifyKeyValue f (KeyValue mVar) = do
  -- Take the MVar, then place an unevaluated thunk inside of it, so
  -- we don't hold onto the MVar for long
  m <- takeMVar mVar
  let m' = f m
  putMVar mVar m'
  -- Force evaluation of the thunk so that we don't build up a large
  -- chain of thunks
  seq m' (pure (KeyValue mVar))
