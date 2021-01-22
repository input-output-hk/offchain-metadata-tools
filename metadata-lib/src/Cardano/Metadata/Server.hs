
module Cardano.Metadata.Server
  ( ReadFns(..)
  , metadataServer
  , webApp
  ) where

import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import           Network.Wai                  ( Application )
import Data.Proxy (Proxy)
import Servant

import Cardano.Metadata.Server.Types
import Cardano.Metadata.Server.API

-- | A set of functions that allows the user of this library to
-- determine how metadata entries are retrieved. E.g. with postgres or
-- with dynamo-db.
data ReadFns = ReadFns { readEntry    :: Subject -> IO Entry
                  -- ^ Given a subject, return an Entry
                  , readProperty :: Subject -> Text -> IO AnyProperty
                  -- ^ Return the given property for the given subject
                  , readBatch    :: BatchRequest -> IO BatchResponse
                  -- ^ Service a batch request
                  }

-- | 'Network.Wai.Application' of the metadata server.
--
-- The function takes a set of functions as an argument, that
-- determine how the application will service requests.
webApp :: ReadFns -> Application
webApp fns = serve (Proxy :: Proxy MetadataServerAPI) (metadataServer fns)

metadataServer :: ReadFns -> Server MetadataServerAPI
metadataServer fns = subjectHandler (readEntry fns)
                :<|> subjectHandler (readEntry fns)
                :<|> propertyHandler (readProperty fns)
                :<|> batchHandler (readBatch fns)

subjectHandler
  :: (Subject -> IO Entry)
  -> Subject
  -> Handler Entry
subjectHandler f subject = liftIO $ f subject

propertyHandler
  :: (Subject -> Text -> IO AnyProperty)
  -> Subject
  -> Text
  -> Handler AnyProperty
propertyHandler f subject property = liftIO $ f subject property

batchHandler
  :: (BatchRequest -> IO BatchResponse)
  -> BatchRequest
  -> Handler BatchResponse
batchHandler f req = liftIO $ f req
