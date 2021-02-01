
module Cardano.Metadata.Store.Simple where

import Prelude hiding (read, init)
import Control.Concurrent.MVar
import Data.Map.Strict (Map)
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map

import Cardano.Metadata.Server.Types
import Cardano.Metadata.Store.Types 

-- readFns :: ReadFns
-- readFns =
--   ReadFns
--     (pure . getEntryForSubject)
--     (\subj -> pure . getPartialEntryForProperty subj)
--     (pure . getBatch)

-- getEntryForSubject :: Subject -> Either ReadError Entry
-- getEntryForSubject subj = case Map.lookup subj dat of
--   Nothing -> Left $ NoSubject subj
--   Just e  -> Right e

-- getPartialEntryForProperty :: Subject -> Text -> Either ReadError PartialEntry
-- getPartialEntryForProperty subj prop = do
--   entry <- getEntryForSubject subj
--   getProperty subj prop entry

-- getBatch :: BatchRequest -> BatchResponse
-- getBatch (BatchRequest subjs props) =
--   BatchResponse $
--     flip foldMap subjs $ \subj ->
--       flip foldMap props $ \prop ->
--         case getPartialEntryForProperty subj prop of
--           Left _err -> []
--           Right x   -> [x]

newtype KeyValue k v = KeyValue (MVar (Map k v))

simpleStore :: Ord k => Map k v -> IO (StoreInterface k v)
simpleStore state = do
  kvs <- init state
  pure $ StoreInterface (\k   -> read k kvs)
                        (\k v -> void $ write k v kvs)
                        (\k   -> void $ delete k kvs)
                        (\f k -> void $ update f k kvs)
                        (toList kvs)
                        (void $ empty kvs)

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

update :: Ord k => (v -> Maybe v) -> k -> KeyValue k v -> IO (KeyValue k v)
update fv k = modifyKeyValue (Map.update fv k)

toList :: KeyValue k v -> IO [(k, v)]
toList (KeyValue mVar) = do
  m <- readMVar mVar
  pure $ Map.toList m

empty :: Ord k => KeyValue k v -> IO (KeyValue k v)
empty = modifyKeyValue (const mempty)

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
