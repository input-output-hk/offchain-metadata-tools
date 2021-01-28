{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Metadata.Store.Types where

import Data.Text (Text)

import Cardano.Metadata.Server.Types

-- | A set of functions that allows the user of this library to
-- determine how metadata entries are retrieved. E.g. with postgres or
-- with dynamo-db.
data ReadFns
  = ReadFns { readEntry    :: Subject -> IO (Either ReadError Entry)
            -- ^ Given a subject, return an Entry
            , readProperty :: Subject -> Text -> IO (Either ReadError PartialEntry)
            -- ^ Return the given property for the given subject
            , readBatch    :: BatchRequest -> IO BatchResponse
            -- ^ Service a batch request
            }

data ReadError = NoSubject Subject
               | NoProperty Subject Text

data WriteFns
  = WriteFns { writeEntry :: Entry -> IO ()
             -- ^ Given an entry, write it to the data store
             , removeEntry :: Subject -> IO ()
             -- ^ Given a subject, remove it's entry from the data store
             , writeBatch  :: [Entry] -> IO ()
             , removeAll   :: IO ()
             }

-- | Any metadata storage backend may be used (in-memory, Postgres,
-- DynamoDB), so long as it conforms to the following interface. A
-- suite of tests is available for this interface and users writing
-- their own store implementation should use these tests to ensure the
-- data store has the correct semantics.
data StoreInterface k v
  = StoreInterface { storeRead   :: k -> IO (Maybe v)
                   -- ^ read: Read a value from the data store
                   , storeWrite  :: k -> v -> IO ()
                   -- ^ write: Write a value to the data store
                   , storeDelete :: k -> IO ()
                   -- ^ delete: Remove a key value pair from the data store
                   , storeUpdate :: (v -> Maybe v) -> k -> IO ()
                   -- ^ update: Read a value from the data store and
                   -- apply a function to it. If the result is
                   -- Nothing, delete the key, otherwise update the
                   -- value at that key with the result of the
                   -- function application (removing the "Just").
                   , storeToList :: IO [(k, v)]
                   -- ^ toList: Convert a data store to a list of
                   -- key-value pairs.
                   , storeEmpty  :: IO ()
                   -- ^ empty: Delete all entries in the key-value store
                   }
