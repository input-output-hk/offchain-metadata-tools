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

import           Prelude hiding (read)
import           Control.Exception.Safe        (catchAny)
import           Control.Monad.IO.Class        (liftIO)
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8    as BLC
import qualified Data.HashMap.Strict           as HM
import           Data.Text                     (Text)
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TLE
import           Servant

import           Cardano.Metadata.Server.API
import           Cardano.Metadata.Server.Types (BatchRequest (BatchRequest),
                                                BatchResponse (BatchResponse))
import           Cardano.Metadata.Store.Types
import           Cardano.Metadata.Types.Common (PropertyName, Subject (Subject),
                                                unPropertyName)
import           Cardano.Metadata.Types.Weakly (Metadata (Metadata),
                                                getMetadataProperty,
                                                metaSubject)

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
    metadataView <- narrowPropertiesThrowingErrors [propName] entry
    pure $ projectMetadataFromView (metaSubject entry) metadataView

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
              metadataView = narrowPropertiesIgnoringErrors propNames entry
            in
              [projectMetadataFromView (metaSubject entry) metadataView]

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

-- | Narrow a list of properties from the metadata, treating any error as
-- a failure.
narrowPropertiesThrowingErrors :: [PropertyName] -> Metadata -> Either ReadError MetadataView
narrowPropertiesThrowingErrors ps = fmap mconcat . sequence . narrowProperties ps

-- | Narrow a list of properties from the metadata, ignoring any errors
-- thrown.
narrowPropertiesIgnoringErrors :: [PropertyName] -> Metadata -> MetadataView
narrowPropertiesIgnoringErrors ps = foldMap mconcat . fmap ignoreErrors . narrowProperties ps
  where
    ignoreErrors :: Either a b -> [b]
    ignoreErrors (Left _err) = []
    ignoreErrors (Right x)   = [x]

-- | Get a list of properties from the metadata, ignoring requests for
-- the metadata subject.
narrowProperties :: [PropertyName] -> Metadata -> [Either ReadError MetadataView]
narrowProperties ps metadata =
  flip foldMap ps $ \p ->
    [narrowProperty p metadata]

-- | Narrow a Metadata entry to the requested property.
narrowProperty :: PropertyName -> Metadata -> Either ReadError MetadataView
narrowProperty "subject" _meta =
  Right $ MetadataView mempty
narrowProperty propName meta =
  case getMetadataProperty propName meta of
    Nothing   -> Left $ NoProperty (metaSubject meta) propName
    Just prop -> Right $ MetadataView (HM.singleton propName prop)

data MetadataView = MetadataView { mvProps    :: HM.HashMap PropertyName Aeson.Value
                                 }
  deriving (Eq, Show)

projectMetadataFromView :: Subject -> MetadataView -> Metadata
projectMetadataFromView subj (MetadataView props) = Metadata subj props

instance Semigroup MetadataView where
  (MetadataView ps1) <> (MetadataView ps2) =
    (MetadataView (HM.unionWith (\_ b -> b) ps1 ps2))

instance Monoid MetadataView where
  mempty = MetadataView mempty
