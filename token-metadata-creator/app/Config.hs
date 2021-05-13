{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Config where

import Prelude

import Cardano.Metadata.GoguenRegistry
    ( GoguenRegistryEntry (..), PartialGoguenRegistryEntry )
import Cardano.Metadata.Types
    ( Subject (..), WellKnownProperty (..), emptyAttested )
import Colog
    ( pattern D, pattern E, pattern I, Severity, pattern W )
import Control.Applicative
    ( empty, optional, (<|>) )
import Data.Foldable
    ( asum )
import Data.List
    ( isSuffixOf )
import Data.Text
    ( Text )
import Data.Void
    ( Void )
import qualified Options.Applicative as OA

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Text as T
import qualified Text.Megaparsec as P

type Parser = P.Parsec Void Text

data DraftStatus
    = DraftStatusDraft
    | DraftStatusFinal
    deriving Show

data EntryOperation
    = EntryOperationInitialize
    | EntryOperationRevise
    deriving Show

data AttestationField
    = AttestationFieldName
    | AttestationFieldDescription
    | AttestationFieldLogo
    | AttestationFieldUrl
    | AttestationFieldTicker
    | AttestationFieldDecimals
    deriving (Show, Eq, Ord)

data FileInfo = FileInfo
    { _FileInfoSubject :: Subject
    , _FileInfoEntryOperation :: EntryOperation
    , _FileInfoDraftStatus :: DraftStatus
    }
  deriving Show

data EntryUpdateArguments = EntryUpdateArguments
    { _EntryUpdateArgumentsFileInfo :: FileInfo
    , _EntryUpdateArgumentsAttestationKeyFilename :: Maybe String
    , _EntryUpdateArgumentsAttestationFields :: [AttestationField]
    , _EntryUpdateArgumentsRegistryEntry :: PartialGoguenRegistryEntry
    , _EntryUpdateLogoFilename :: Maybe String
    , _EntryUpdatePolicyFilenameOrCBOR :: Maybe String
    }
    deriving Show

data Arguments
    = ArgumentsEntryUpdate EntryUpdateArguments
    | ArgumentsValidate FilePath (Maybe FilePath) Severity
    deriving Show

argumentParser :: Maybe Subject -> OA.Parser Arguments
argumentParser defaultSubject =
  OA.hsubparser 
    ( OA.command "entry" (OA.info (ArgumentsEntryUpdate <$> entryUpdateArgumentParser defaultSubject) mempty)
   <> OA.command "validate" (OA.info (ArgumentsValidate <$> pFileA <*> pFileB <*> pLogSeverity) mempty)
    )

  where
    pFileA :: OA.Parser FilePath
    pFileA = OA.strArgument (OA.metavar "FILE" <> OA.help "File to validate")

    pFileB :: OA.Parser (Maybe FilePath)
    pFileB = optional (OA.strArgument (OA.metavar "FILE" <> OA.help "If present, validates this file instead of the first and also validates the difference between the first and second files."))

entryUpdateArgumentParser :: Maybe Subject -> OA.Parser EntryUpdateArguments
entryUpdateArgumentParser defaultSubject = EntryUpdateArguments
    <$> fileInfoArgumentParser
    <*> optional (OA.strOption (OA.long "attest-keyfile" <> OA.short 'a' <> OA.metavar "ATTESTATION_KEY_FILE"))
    <*> attestationFieldNamesParser
    <*> goguenRegistryEntryParser
    <*> logoFilenameParser
    <*> policyParser
  where
    attestationFieldNamesParser :: OA.Parser [AttestationField]
    attestationFieldNamesParser = asum
        [ OA.flag' [AttestationFieldName] $ OA.long "attest-name" <> OA.short 'N'
        , OA.flag' [AttestationFieldDescription] $ OA.long "attest-description" <> OA.short 'D'
        , OA.flag' [AttestationFieldLogo] $ OA.long "attest-logo" <> OA.short 'L'
        , OA.flag' [AttestationFieldUrl] $ OA.long "attest-url" <> OA.short 'H'
        , OA.flag' [AttestationFieldTicker] $ OA.long "attest-ticker" <> OA.short 'T'
        , OA.flag' [AttestationFieldDecimals] $ OA.long "attest-decimals"
        , pure
            [ AttestationFieldName
            , AttestationFieldDescription
            , AttestationFieldLogo
            , AttestationFieldUrl
            , AttestationFieldTicker
            , AttestationFieldDecimals
            ]
       ]

    fileInfoArgumentParser :: OA.Parser FileInfo
    fileInfoArgumentParser = FileInfo
        <$> (trimSubject <$> OA.strArgument (OA.metavar "SUBJECT") <|> defaultSubjectParser)
        <*> OA.flag EntryOperationRevise EntryOperationInitialize (OA.long "init" <> OA.short 'i')
        <*> OA.flag DraftStatusDraft DraftStatusFinal (OA.long "finalize" <> OA.short 'f')

    defaultSubjectParser =
        maybe empty pure defaultSubject

    trimSubject :: String -> Subject
    trimSubject subj
        | jsonSuffix `isSuffixOf` subj =
            Subject $ T.pack $ take (length subj - length jsonSuffix) subj
        | jsonDraftSuffix `isSuffixOf` subj =
            Subject $ T.pack $ take (length subj - length jsonDraftSuffix) subj
        | otherwise =
            Subject $ T.pack subj

    logoFilenameParser :: OA.Parser (Maybe String)
    logoFilenameParser = optional $ OA.strOption (OA.long "logo" <> OA.short 'l' <> OA.metavar "LOGO.png")

    policyParser :: OA.Parser (Maybe String)
    policyParser = optional (OA.strOption (OA.long "policy" <> OA.short 'p' <> OA.metavar "POLICY"))

    goguenRegistryEntryParser :: OA.Parser (PartialGoguenRegistryEntry)
    goguenRegistryEntryParser = GoguenRegistryEntry Nothing Nothing
        <$> optional (emptyAttested <$> wellKnownOption (OA.long "name" <> OA.short 'n' <> OA.metavar "NAME"))
        <*> optional (emptyAttested <$> wellKnownOption (OA.long "description" <> OA.short 'd' <> OA.metavar "DESCRIPTION"))
        <*> pure Nothing -- logo
        <*> optional (emptyAttested <$> wellKnownOption (OA.long "url" <> OA.short 'h' <> OA.metavar "URL"))
        <*> optional (emptyAttested <$> wellKnownOption (OA.long "ticker" <> OA.short 't' <> OA.metavar "TICKER"))
        <*> optional (emptyAttested <$> OA.option (OA.eitherReader ((Aeson.parseEither parseWellKnown =<<) . Aeson.eitherDecodeStrict . BC8.pack)) (OA.long "decimals" <>  OA.metavar "DECIMALS"))

pLogSeverity :: OA.Parser Colog.Severity
pLogSeverity = pDebug <|> pInfo <|> pWarning <|> pError <|> pure I
  where
    pDebug =
      OA.flag' D
        (  OA.long "debug"
        <> OA.help "Print debug, info, warning, and error messages"
        )
    pInfo =
      OA.flag' I
        (  OA.long "info"
        <> OA.help "Print info, warning, and error messages"
        )
    pWarning =
      OA.flag' W
        (  OA.long "warning"
        <> OA.help "Print warning, and error messages"
        )
    pError =
      OA.flag' E
        (  OA.long "error"
        <> OA.help "Print error messages only"
        )

canonicalFilename :: FileInfo -> String
canonicalFilename = (<> jsonSuffix) . T.unpack . unSubject . _FileInfoSubject

jsonSuffix, draftSuffix, jsonDraftSuffix :: String
jsonSuffix = ".json"
draftSuffix = ".draft"
jsonDraftSuffix = jsonSuffix <> draftSuffix

draftFilename :: FileInfo -> String
draftFilename fi = canonicalFilename fi <> draftSuffix

wellKnownOption
    :: forall p. WellKnownProperty p
    => OA.Mod OA.OptionFields p
    -> OA.Parser p
wellKnownOption =
    OA.option wellKnownReader
  where
    wellKnownReader :: OA.ReadM p
    wellKnownReader = OA.eitherReader $
        Aeson.parseEither parseWellKnown . Aeson.toJSON
