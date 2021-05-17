{-# LANGUAGE OverloadedStrings #-}

module Cardano.Metadata.Sync where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Turtle.Prelude as Turtle
import System.Directory (listDirectory)
import System.FilePath.Posix (takeBaseName)
import Data.String (fromString)
import System.IO.Temp (withSystemTempDirectory)
import Database.PostgreSQL.Simple (withTransaction, execute, executeMany, Connection)
import Database.PostgreSQL.Simple.Types (Identifier(..), Only(..), In(..))
import Data.Traversable (forM)
import Data.Functor (void)

import Cardano.Metadata.Sync.Config (withConnectionFromPool)
import Cardano.Metadata.Types.Common (Subject(..))

-- | View the current state of the registry (source-of-truth).
view :: Text -> Text -> IO [(Subject, Aeson.Value)]
view gitURL gitSubFolder = do
  withSystemTempDirectory "metadata-sync" $ \dir -> do

    gitURL `cloneTo` dir

    let dataDir = dir <> "/" <> T.unpack gitSubFolder
    ks <- listDirectory dataDir
    flip foldMap ks $ \k -> do
      mV <- Aeson.decodeFileStrict' (dataDir <> "/" <> k)
      case mV of
        Nothing -> pure []
        Just v -> pure [(Subject $ T.pack $ takeBaseName k, v)]

  where
    emptyDirectory dir = Turtle.procs "rm" ["-r", T.pack dir <> "/*"] mempty
    cloneTo gitUrl dir = Turtle.procs "git" ["clone", gitUrl, T.pack dir] mempty

-- | Write out a new state to our local copy of the registry.
write :: Connection -> Text -> [(Subject, Aeson.Value)] -> IO ()
write conn tableName kvs =
  withTransaction conn $ do
    let table = Identifier tableName

    void $ execute conn "TRUNCATE ?" (Only table)

    let dat = fmap (\(Subject k, v) -> (k, v)) kvs

    void $ executeMany conn ("INSERT INTO " <> fromString (T.unpack tableName) <> " VALUES (?,?)") dat
