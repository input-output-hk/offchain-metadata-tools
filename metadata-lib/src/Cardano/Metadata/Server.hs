{-# LANGUAGE OverloadedStrings #-}

module Cardano.Metadata.Server
  ( ReadFns(..)
  , ReadError(..)
  , metadataServer
  , webApp
  , MetadataServerAPI
  ) where

import Data.Text (Text)
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class (liftIO)
import           Network.Wai                  ( Application )
import Data.Proxy (Proxy)
import Servant
import Control.Exception.Safe (catchAny)

import Cardano.Metadata.Server.Types
import Cardano.Metadata.Store.Types
import Cardano.Metadata.Server.API

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
  :: (Subject -> IO (Either ReadError Entry))
  -> Subject
  -> Handler Entry
subjectHandler f subject = catchExceptions $ handleErrors =<< liftIO (f subject)

propertyHandler
  :: (Subject -> Text -> IO (Either ReadError PartialEntry))
  -> Subject
  -> Text
  -> Handler PartialEntry
propertyHandler f subject property = catchExceptions $ handleErrors =<< liftIO (f subject property)

batchHandler
  :: (BatchRequest -> IO BatchResponse)
  -> BatchRequest
  -> Handler BatchResponse
batchHandler f req = catchExceptions $ liftIO $ f req

handleErrors :: Either ReadError a -> Handler a
handleErrors r =
  case r of
    (Left (NoSubject subj))       -> throwError $ err404 { errBody = "Requested subject '" <> c subj <> "' not found" }
    (Left (NoProperty subj prop)) -> throwError $ err404 { errBody = "Requested subject '" <> c subj <> "' does not have the property '" <> c prop <> "'" }
    (Right x)                     -> pure x
    
  where
    c :: Text -> BL.ByteString
    c = TLE.encodeUtf8 . TL.fromStrict

catchExceptions :: Handler a -> Handler a
catchExceptions action =
  action
    `catchAny`
      (\e -> throwError $ err500 { errBody = "Exception occurred while handling request: " <> BLC.pack (show e) <> "." } )
