{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Cardano.Metadata.Server
  ( ReadError(..)
  , metadataServer
  , webApp
  , MetadataServerAPI
  ) where

import Data.Text (Text)
import Data.Monoid(First(First))
import Data.Traversable (forM)
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text.Lazy as TL
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
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
webApp :: StoreInterface Subject Entry' -> Application
webApp intf = serve (Proxy :: Proxy MetadataServerAPI) (metadataServer intf)

metadataServer :: StoreInterface Subject Entry' -> Server MetadataServerAPI
metadataServer intf = subjectHandler intf
                 :<|> subjectHandler intf
                 :<|> propertyHandler intf
                 :<|> batchHandler intf

subjectHandler
  :: StoreInterface Subject Entry'
  -> Subject
  -> Handler Entry'
subjectHandler (StoreInterface { storeRead = read }) subject =
  catchExceptions . handleErrors =<< liftIO (do
    mEntry <- read subject
    pure $ case mEntry of
      Nothing    -> Left $ NoSubject subject
      Just entry -> Right entry
  )

propertyHandler
  :: StoreInterface Subject Entry'
  -> Subject
  -> PropertyName
  -> Handler PartialEntry'
propertyHandler f subject propName = do
  entry <- subjectHandler f subject
  catchExceptions . handleErrors $ do
    partialEntry <- getProperty subject propName entry
    pure $ PartialEntry' subject partialEntry

batchHandler
  :: StoreInterface Subject Entry'
  -> BatchRequest
  -> Handler BatchResponse
batchHandler (StoreInterface { storeReadBatch = readBatch }) (BatchRequest subjects propNames) =
  catchExceptions . liftIO $ do
    entries <- readBatch subjects

    pure $ BatchResponse $ 
      flip foldMap entries $ \entry@(Entry' subject _) ->
        let
          partialEntry = flip foldMap propNames $ \propName ->
            getPropertyLenient subject propName entry
        in
          [PartialEntry' subject partialEntry]

handleErrors :: Either ReadError a -> Handler a
handleErrors r =
  case r of
    (Left (NoSubject subj))       -> throwError $ err404 { errBody = "Requested subject '" <> c subj <> "' not found" }
    (Left (NoProperty subj prop)) -> throwError $ err404 { errBody = "Requested subject '" <> c subj <> "' does not have the property '" <> c (getPropertyName prop) <> "'" }
    (Right x)                     -> pure x
    
  where
    c :: Text -> BL.ByteString
    c = TLE.encodeUtf8 . TL.fromStrict

catchExceptions :: Handler a -> Handler a
catchExceptions action =
  action
    `catchAny`
      (\e -> throwError $ err500 { errBody = "Exception occurred while handling request: " <> BLC.pack (show e) <> "." } )

getProperty :: Subject -> PropertyName -> Entry' -> Either ReadError PartialEntry
getProperty subj propName entry =
  case getPropertyLenient subj propName entry of
    x | x == mempty
       && propName /= propertyName PropTypeSubject
      -> Left $ NoProperty subj propName
    x -> Right x

getPropertyLenient :: Subject -> PropertyName -> Entry' -> PartialEntry
getPropertyLenient subj propName = withProperties f
  where
    f :: forall t. Property t -> PartialEntry
    f (PropOwner t owner)             | (propName == propertyName t) = PartialEntry $ mempty { enOwner = (First $ Just owner) }
    f (PropName t name)               | (propName == propertyName t) = PartialEntry $ mempty { enName = (First $ Just name) }
    f (PropDescription t description) | (propName == propertyName t) = PartialEntry $ mempty { enDescription = (First $ Just description) }
    f (PropPreImage t preImage)       | (propName == propertyName t) = PartialEntry $ mempty { enPreImage = (First $ Just preImage) }
    f otherwise                                                      = mempty
