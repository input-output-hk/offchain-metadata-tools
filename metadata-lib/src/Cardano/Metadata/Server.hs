{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Cardano.Metadata.Server
  ( ReadError(..)
  , metadataServer
  , webApp
  , MetadataServerAPI
  ) where

import           Control.Exception.Safe        (catchAny)
import           Control.Monad.IO.Class        (liftIO)
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BLC
import           Data.Functor.Identity         (Identity (Identity))
import qualified Data.HashMap.Strict           as HM
import           Data.Monoid                   (First (First))
import           Data.Proxy                    (Proxy)
import           Data.Text                     (Text)
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TLE
import           Data.Traversable              (forM)
import           Network.Wai                   (Application)
import           Servant

import           Cardano.Metadata.Server.API
import           Cardano.Metadata.Server.Types (BatchRequest (BatchRequest),
                                                BatchResponse (BatchResponse))
import           Cardano.Metadata.Store.Types
import           Cardano.Metadata.Types.Common (PropertyName, Subject (Subject),
                                                unPropertyName)
import           Cardano.Metadata.Types.Weakly (Metadata (Metadata), Property,
                                                getMetadataProperty,
                                                metaProperties, metaSubject)

-- | 'Network.Wai.Application' of the metadata server.
--
-- The function takes a set of functions as an argument, that
-- determine how the application will service requests.
webApp :: StoreInterface Subject Metadata -> Application
webApp intf = serve (Proxy :: Proxy MetadataServerAPI) (metadataServer intf)

metadataServer :: StoreInterface Subject Metadata -> Server MetadataServerAPI
metadataServer intf = subjectHandler intf
                 :<|> subjectHandler intf
                 :<|> propertyHandler intf
                 :<|> batchHandler intf

subjectHandler
  :: StoreInterface Subject Metadata
  -> Subject
  -> Handler Metadata
subjectHandler (StoreInterface { storeRead = read }) subject =
  catchExceptions . handleErrors =<< liftIO (do
    mMetadata <- read subject
    pure $ case mMetadata of
      Nothing    -> Left $ NoSubject subject
      Just entry -> Right entry
  )

propertyHandler
  :: StoreInterface Subject Metadata
  -> Subject
  -> PropertyName
  -> Handler Metadata
propertyHandler f subject propName = do
  entry <- subjectHandler f subject
  catchExceptions . handleErrors $ do
    props <- getPropertiesThrowingErrors [propName] entry
    pure $ Metadata subject (HM.fromList props)

batchHandler
  :: StoreInterface Subject Metadata
  -> BatchRequest
  -> Handler BatchResponse
batchHandler (StoreInterface { storeReadBatch = readBatch }) (BatchRequest subjects mPropNames) =
  catchExceptions . liftIO $ do
    entries <- readBatch subjects

    pure $ BatchResponse $
      flip foldMap entries $ \entry ->
        case mPropNames of
          Nothing        -> [entry]
          Just propNames ->
            let
              props = getPropertiesIgnoringErrors propNames entry
            in
              [Metadata (metaSubject entry) (HM.fromList props)]

handleErrors :: Either ReadError a -> Handler a
handleErrors r =
  case r of
    (Left (NoSubject (Subject subj)))       -> throwError $ err404 { errBody = "Requested subject '" <> c subj <> "' not found" }
    (Left (NoProperty (Subject subj) prop)) -> throwError $ err404 { errBody = "Requested subject '" <> c subj <> "' does not have the property '" <> c (unPropertyName prop) <> "'" }
    (Right x)                               -> pure x

  where
    c :: Text -> BL.ByteString
    c = TLE.encodeUtf8 . TL.fromStrict

catchExceptions :: Handler a -> Handler a
catchExceptions action =
  action
    `catchAny`
      (\e -> throwError $ err500 { errBody = "Exception occurred while handling request: " <> BLC.pack (show e) <> "." } )

data PropertyResponse = RequestedSubject Subject
                      | RequestedProperty Property

-- | Get a list of properties from the metadata, treating any error as
-- a failure.
getPropertiesThrowingErrors :: [PropertyName] -> Metadata -> Either ReadError [(PropertyName, Property)]
getPropertiesThrowingErrors ps = sequence . getProperties ps

-- | Get a list of properties from the metadata, ignoring any errors
-- thrown.
getPropertiesIgnoringErrors :: [PropertyName] -> Metadata -> [(PropertyName, Property)]
getPropertiesIgnoringErrors ps = mconcat . fmap ignoreErrors . getProperties ps
  where
    ignoreErrors :: Either a b -> [b]
    ignoreErrors (Left _err) = []
    ignoreErrors (Right x)   = [x]

-- | Get a list of properties from the metadata, ignoring requests for
-- the metadata subject.
getProperties :: [PropertyName] -> Metadata -> [Either ReadError (PropertyName, Property)]
getProperties ps metadata =
  flip foldMap ps $ \p ->
    case getProperty p metadata of
      Left err    -> [Left err]
      Right mProp -> case mProp of
        RequestedSubject _subj -> mempty
        RequestedProperty prop -> [Right (p, prop)]

-- | Get a property from the metadata.
getProperty :: PropertyName -> Metadata -> Either ReadError PropertyResponse
getProperty "subject" metadata = Right $ RequestedSubject (metaSubject metadata)
getProperty propName metadata  =
  maybe (Left $ NoProperty (metaSubject metadata) propName) (pure . RequestedProperty) $ getMetadataProperty propName metadata
