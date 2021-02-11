{-# LANGUAGE FlexibleInstances #-}
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
import Data.Function ((&))
import GHC.Int (Int64)
import Data.Maybe (fromJust)
import Data.Void (Void)
import Data.Char (isHexDigit)
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

import Cardano.Metadata.Server.Types (Entry')
import Config (Config(Config), opts, mkConfig, AuthScheme(NoAuthScheme, OAuthScheme))

type Parser = P.Parsec Void Text

pFileNameHex :: Parser Text
pFileNameHex = do
  (digits :: Text) <- P.takeWhile1P (Just "hex digit/char") isHexDigit

  let numDigits = T.length digits
  if numDigits < 56 || numDigits > 64
    then fail $ "Expected 56-64 hex digits but found " <> show numDigits <> "."
    else pure ()

  _fileSuffix <- P.string ".json"
  P.eof
  pure digits

main :: IO ()
main = do
  (Config authScheme repoOwner repoName prNumber)
    <- mkConfig <$> Opt.execParser opts

  let
    run :: forall req. FromJSON req => GitHub.Request ('GitHub.RO :: GitHub.RW) req -> IO req
    run =
      let
        go = case authScheme of
          NoAuthScheme      -> GitHub.github'
          OAuthScheme token -> GitHub.github (GitHub.OAuth token)
      in
        (either (error . show) pure =<<) . go

  pr      <- run $ GitHub.pullRequestR repoOwner repoName prNumber
  prFiles <- run $ GitHub.pullRequestFilesR repoOwner repoName prNumber (GitHub.FetchAtLeast 1)

  let
    baseBranch   = pr & GitHub.pullRequestBase & GitHub.pullRequestCommitRef
    changedFiles = pr & GitHub.pullRequestChangedFiles

  if baseBranch /= "master"
    then error $ "Wanted base branch 'master' but got '" <> T.unpack baseBranch <> "'."
    else pure ()

  if changedFiles /= 1
    then error $ "Pull request may only add or modify one file, but it changed " <> show changedFiles <> " files."
    else pure ()

  case Vector.toList prFiles of
    []      -> error $ "No files changed!"
    file:[] -> do
      putStrLn $ show file
      let fileName = GitHub.fileFilename file
      case P.runParser pFileNameHex (T.unpack fileName) fileName of
        Left err -> error $ "Failed to parse file name, error was: '" <> P.errorBundlePretty err <> "'."
        Right _ ->
          case GitHub.fileStatus file of
            "removed"  -> error "Removing a record is not permitted."
            "renamed"  -> error "Renaming a record is not permitted."
            "modified" -> putStrLn "Modifying a record..."
            "added"    -> putStrLn "Adding a record..."
            x          -> error $ "Unknown status '" <> T.unpack x <> "' is not permitted."

      blob <- run $ GitHub.blobR repoOwner repoName (GitHub.N $ fromJust $ GitHub.fileSha file)
      putStrLn $ show blob
      let
        content = BSL.fromStrict $ Base64.decodeLenient $ TE.encodeUtf8 $ GitHub.blobContent blob
        contentLength = BSL.length content

      if contentLength > metadataJSONMaxSize
        then error $ "File size in bytes (" <> show contentLength <> ") greater than maximum size of " <> show metadataJSONMaxSize <> " bytes."
        else putStrLn $ "Good file size ( " <> show contentLength <> " < " <> show metadataJSONMaxSize <> " bytes)"

      case Aeson.eitherDecode content of
        Left err                   -> error $ "Content '" <> BSLC.unpack content <> "' is not a valid JSON value, decoding error was: '" <> show err <> "'."
        Right (obj :: Aeson.Value) -> do
          case Aeson.parseEither Aeson.parseJSON obj of
            Left err           -> error $ "Failed to decode Metadata entry from JSON value '" <> show obj <> "', error was: '" <> show err <> "'."
            Right (entry :: Entry') -> do
              putStrLn $ "PR valid!"
              putStrLn $ "Decoded entry: " <> show entry
    _files  -> error $ "Too many files changed!"

-- | Maximum size in bytes of a metadata entry.
metadataJSONMaxSize :: Int64
metadataJSONMaxSize = 380000
