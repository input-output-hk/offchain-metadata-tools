{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Metadata.Store.File
  ( read
  , write
  , update
  , delete
  , empty
  , toList
  , init
  , fileStore
  ) where

import Cardano.Metadata.Store.Types
import Control.Exception.Safe
import Control.Monad.Reader
import Data.Aeson
    ( FromJSON, FromJSONKey, ToJSON, ToJSONKey )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding.Internal as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Coerce
    ( coerce )
import Data.Maybe
    ( catMaybes, fromMaybe )
import Data.Text
    ( Text )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Prelude hiding
    ( init, read )
import System.Directory
import System.FilePath.Posix
    ( takeFileName )

data PostgresKeyValueException = UniqueKeyConstraintViolated
                               | FailedToDecodeJSONValue String Text
  deriving (Eq, Show, Exception)

data KeyValue k v = KeyValue { _kvFolder :: FilePath }

init
  :: FilePath
  -- ^ Folder containing metadata entries
  -> IO (KeyValue k v)
  -- ^ Resulting key-value store
init = pure . KeyValue

fileStore
  :: ( ToJSONKey k
     , ToJSON v
     , FromJSONKey k
     , FromJSON v
     )
  => FilePath
  -- ^ Folder containing metadata entries
  -> IO (StoreInterface k v)
fileStore folder = do
  let kvs = KeyValue folder
  pure $ StoreInterface (\k   -> read k kvs)
                        (\ks  -> readBatch ks kvs)
                        (\k v -> write k v kvs)
                        (\k   -> delete k kvs)
                        (\f k -> update f k kvs)
                        (toList kvs)
                        (empty kvs)

-- | Ensure file path is within folder.
safeFilePath :: ToJSONKey k => KeyValue k v -> k -> FilePath
safeFilePath (KeyValue folder) k =
  let
    -- Disallow user to enter a sub-directory or a parent directory by
    -- limiting the requested path to a file name. I.e. "../x.txt" and
    -- "inner/x.txt" are normalised to "x.txt" to restrict the user
    -- from looking outside the specified folder.
    raw :: FilePath
    raw = takeFileName . T.unpack $ toJSONKeyText k
  in
    folder <> "/" <> raw

withFileIfExists :: ToJSONKey k => KeyValue k v -> k -> (FilePath -> IO r) -> IO (Maybe r)
withFileIfExists kvs k f = do
  let safe = safeFilePath kvs k
  exists <- doesFileExist safe
  if exists
    then do
      r <- f safe
      pure $ Just r
    else pure Nothing

read :: (ToJSONKey k, FromJSON v) => k -> KeyValue k v -> IO (Maybe v)
read k kvs = do
  withFileIfExists kvs k $ \safe ->
    Aeson.eitherDecodeFileStrict' safe
      >>= (\v -> handleJSONDecodeError (T.pack safe) v) 

readBatch :: (ToJSONKey k, FromJSON v) => [k] -> KeyValue k v -> IO [v]
readBatch [] _kvs = pure []
readBatch ks kvs  = fmap catMaybes $ forM ks (\k -> read k kvs)

write :: (ToJSONKey k, ToJSON v) => k -> v -> KeyValue k v -> IO ()
write k v kvs =
  let
    safe = safeFilePath kvs k
  in
    Aeson.encodeFile safe v

delete :: ToJSONKey k => k -> KeyValue k v -> IO ()
delete k kvs = 
  fromMaybe () <$> withFileIfExists kvs k removeFile 

update :: (ToJSONKey k, ToJSON v, FromJSON v) => (v -> Maybe v) -> k -> KeyValue k v -> IO ()
update fv k kvs = do
  mv <- read k kvs
  case mv of
    Nothing -> pure ()
    Just v  -> case fv v of
      Nothing       -> delete k kvs
      Just newValue -> write k newValue kvs

toList :: (ToJSONKey k, FromJSONKey k, FromJSON v) => KeyValue k v -> IO [(k, v)]
toList kvs@(KeyValue folder) = do
  ks <- fmap (fmap T.pack) $ listDirectory folder
  forM ks $ \kText -> do
    k <- handleJSONDecodeError kText $ decodeJSONKey kText
    mV <- read k kvs
    pure $ maybe (error $ "Unable to find file with name '" <> (T.unpack $ toJSONKeyText k) <> "'") (k,) mV

empty :: (FromJSONKey k, ToJSONKey k) => KeyValue k v -> IO ()
empty kvs@(KeyValue folder) = do
  ks <- fmap (fmap T.pack) $ listDirectory folder
  void . forM ks $ \kText -> do
    k <- handleJSONDecodeError undefined $ decodeJSONKey kText
    delete k kvs

handleJSONDecodeError :: Text -> Either String a -> IO a
handleJSONDecodeError t = either (\err -> throw $ FailedToDecodeJSONValue err t) pure

toJSONKeyText :: ToJSONKey k => k -> Text
toJSONKeyText k =
  case Aeson.toJSONKey of
    Aeson.ToJSONKeyText  f _ -> f k
    Aeson.ToJSONKeyValue _ f -> TL.toStrict $ TLE.decodeUtf8 $ Aeson.encodingToLazyByteString $ f k

decodeJSONKey :: FromJSONKey k => Text -> Either String k
decodeJSONKey t = case Aeson.fromJSONKey of
  Aeson.FromJSONKeyCoerce -> pure $ coerce t
  Aeson.FromJSONKeyText f -> pure $ f t
  Aeson.FromJSONKeyTextParser p -> Aeson.parseEither p t
  Aeson.FromJSONKeyValue pv -> do
    (v :: Aeson.Value) <- Aeson.eitherDecode (TLE.encodeUtf8 . TL.fromStrict $ t)
    Aeson.parseEither pv v
