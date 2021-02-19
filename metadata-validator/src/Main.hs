{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Main where

import qualified GitHub as GitHub
import qualified Data.Text as T
import Prelude hiding (log)
import Data.Function ((&))
import Data.Foldable (traverse_)
import GHC.Int (Int64)
import System.Exit (exitFailure, exitSuccess)
import Data.Maybe (fromJust)
import Data.Void (Void)
import Data.Char (isPrint)
import Data.Text (Text)
import qualified Data.Vector as Vector
import qualified GitHub.Data.Name as GitHub
import qualified Options.Applicative as Opt
import qualified Data.ByteString.Base64 as Base64
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text.Encoding as TE
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Colog (msgSeverity, pattern E, pattern D, pattern I, LogAction, Message, WithLog, log, usingLoggerT, cmap, fmtMessage, logTextStdout, filterBySeverity)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Cardano.Metadata.Types.Wallet as Wallet (Metadata)
import Config (Config(Config), opts, mkConfig, AuthScheme(NoAuthScheme, OAuthScheme))

type Parser = P.Parsec Void Text

pFileNameHex :: Parser Text
pFileNameHex = do
  (fileName :: Text) <- P.takeWhile1P (Just "printable Unicode character") (\c -> isPrint c && c /= '.')

  let len = T.length fileName
  if len < 1 || len > 256
    then fail $ "Expected 1-256 chars but found " <> show len <> "."
    else pure ()

  _fileSuffix <- P.string ".json"
  P.eof
  pure fileName

main :: IO ()
main = do
  (Config authScheme repoOwner repoName prNumber logSeverity)
    <- mkConfig <$> Opt.execParser opts

  let
    action :: MonadIO m => LogAction m Message
    action = filterBySeverity logSeverity msgSeverity (cmap fmtMessage logTextStdout)

  usingLoggerT action $ do
    pr      <- run' authScheme $ GitHub.pullRequestR repoOwner repoName prNumber
    log D $ T.pack $ "Retrieved pull request: '" <> show pr <> "'."
    prFiles <- run' authScheme $ GitHub.pullRequestFilesR repoOwner repoName prNumber (GitHub.FetchAtLeast 1)
    log D $ T.pack $ "Retrieved pull request files: '" <> show prFiles <> "'."

    let
      baseBranch   = pr & GitHub.pullRequestBase & GitHub.pullRequestCommitRef
      changedFiles = pr & GitHub.pullRequestChangedFiles

    if baseBranch /= "master"
      then do
        log E $ "Wanted base branch 'master' but got '" <> baseBranch <> "'."
        exitFailure'
      else pure ()

    if changedFiles <= 0
      then do
        log E $ T.pack $ "Pull request must add or modify at least one file, but it changed " <> show changedFiles <> " files."
        exitFailure'
      else pure ()

    log I $ T.pack $ "Validating " <> show changedFiles <> " files."
    traverse_ (validatePRFile authScheme repoOwner repoName) (Vector.toList prFiles)
    exitSuccess'

validatePRFile
  :: (MonadIO m, WithLog env Message m)
  => AuthScheme
  -> GitHub.Name GitHub.Owner
  -> GitHub.Name GitHub.Repo
  -> GitHub.File
  -> m ()
validatePRFile authScheme repoOwner repoName file = do
  let fileName = GitHub.fileFilename file

  log I $ "Validating file " <> fileName
  log D $ "File contents: " <> T.pack (show file)

  case P.runParser pFileNameHex (T.unpack fileName) fileName of
    Left err -> do
      log E $ T.pack $ "Failed to parse file name, error was: '" <> P.errorBundlePretty err <> "'."
      liftIO $ exitFailure
    Right _ ->
      case GitHub.fileStatus file of
        "removed"  -> log E "Removing a record is not permitted." >> exitFailure'
        "renamed"  -> log E "Renaming a record is not permitted." >> exitFailure'
        "modified" -> log I "Modifying a record..."
        "added"    -> log I "Adding a record..."
        x          -> log E ("Unknown status '" <> x <> "' is not permitted.") >> exitFailure'

  blob <- run' authScheme $ GitHub.blobR repoOwner repoName (GitHub.N $ fromJust $ GitHub.fileSha file)
  log D $ "Received blob for file '" <> fileName <> "': " <> (T.pack $ show blob)
  let
    content = BSL.fromStrict $ Base64.decodeLenient $ TE.encodeUtf8 $ GitHub.blobContent blob
    contentLength = BSL.length content

  if contentLength > metadataJSONMaxSize
    then log E ("File size in bytes (" <> (T.pack $ show contentLength) <> ") greater than maximum size of " <> (T.pack $ show metadataJSONMaxSize) <> " bytes.") >> exitFailure'
    else log I ("Good file size ( " <> (T.pack $ show contentLength) <> " < " <> (T.pack $ show metadataJSONMaxSize) <> " bytes)")

  case Aeson.eitherDecode content of
    Left err                   -> do
      log E $ T.pack $ "Content '" <> BSLC.unpack content <> "' is not a valid JSON value, decoding error was: '" <> show err <> "'."
      exitFailure'
    Right (obj :: Aeson.Value) -> do
      case Aeson.parseEither Aeson.parseJSON obj of
        Left err           -> do
          log E $ T.pack $ "Failed to decode Metadata entry from JSON value '" <> show obj <> "', error was: '" <> show err <> "'."
          exitFailure'
        Right (entry :: Wallet.Metadata) -> do
          log I "PR valid!"
          log I $ T.pack $ "Decoded entry: " <> show entry

-- | Maximum size in bytes of a metadata entry.
metadataJSONMaxSize :: Int64
metadataJSONMaxSize = 380000

exitFailure' :: MonadIO m => m a
exitFailure' = liftIO exitFailure

exitSuccess' :: MonadIO m => m a
exitSuccess' = liftIO exitSuccess

run' :: forall env m req. (WithLog env Message m, MonadIO m, FromJSON req) => AuthScheme -> GitHub.Request ('GitHub.RO :: GitHub.RW) req -> m req
run' authScheme req = do
  let
    go = case authScheme of
      NoAuthScheme      -> GitHub.github'
      OAuthScheme token -> GitHub.github (GitHub.OAuth token)

  res <- liftIO $ go req
  case res of
    Left err -> do
      log E $ "GitHub error: '" <> (T.pack $ show err) <> "'."
      exitFailure'
    Right x  -> pure x
