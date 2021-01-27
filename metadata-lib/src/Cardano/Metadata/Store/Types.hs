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
