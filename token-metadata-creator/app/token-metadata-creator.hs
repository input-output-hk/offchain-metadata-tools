{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Cardano.Prelude hiding
    ( log )

import Cardano.Api
    ( AsType (AsPaymentExtendedKey, AsPaymentKey) )
import Cardano.CLI.Shelley.Key
    ( readSigningKeyFile )
import Cardano.CLI.Types
    ( SigningKeyFile (..) )
import Cardano.Metadata.GoguenRegistry
    ( GoguenRegistryEntry (..)
    , PartialGoguenRegistryEntry
    , parseRegistryEntry
    , validateEntry
    )
import Cardano.Metadata.Types
    ( Attested (..)
    , MakeAttestationSignature (..)
    , SomeSigningKey (..)
    , Subject (..)
    , WellKnownProperty (..)
    , emptyAttested
    , hashesForAttestation
    )
import Colog
    ( pattern E
    , pattern I
    , LogAction
    , Message
    , Severity
    , WithLog
    , cmap
    , filterBySeverity
    , fmtMessage
    , log
    , logTextStdout
    , msgSeverity
    , usingLoggerT
    )
import Control.Arrow
    ( left )
import Control.Exception.Safe
    ( handleAny )
import Data.Validation
import System.Directory
    ( doesFileExist, renameFile )
import System.Environment
    ( lookupEnv )
import System.IO
    ( hFileSize )

import Cardano.Metadata.Types.Common
    ( File (File) )
import Cardano.Metadata.Validation.Rules
    ( apply, prettyPrintValidationError )
import Cardano.Metadata.Validation.Types
    ( Difference (Added, Changed), Metadata )
import Cardano.Metadata.Validation.Wallet
    ( prettyPrintWalletValidationError, walletValidationRules )

import Config
    ( Arguments (ArgumentsEntryUpdate, ArgumentsValidate)
    , AttestationField (AttestationFieldDecimals, AttestationFieldDescription, AttestationFieldLogo, AttestationFieldName, AttestationFieldTicker, AttestationFieldUrl)
    , DraftStatus (DraftStatusDraft, DraftStatusFinal)
    , EntryOperation (EntryOperationInitialize, EntryOperationRevise)
    , EntryUpdateArguments (EntryUpdateArguments)
    , FileInfo (..)
    , argumentParser
    , canonicalFilename
    , draftFilename
    )

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Options.Applicative as OA
import qualified System.IO as IO

main :: IO ()
main = do
    defaultSubject <- fmap (Subject . T.pack) <$> lookupEnv "METADATA_SUBJECT"
    args <- OA.execParser $ OA.info (argumentParser defaultSubject <**> OA.helper) mempty
    case args of
        ArgumentsEntryUpdate eua            -> handleEntryUpdateArguments eua
        ArgumentsValidate fpA mFpB severity -> handleValidate severity fpA mFpB

combineRegistryEntries
    :: GoguenRegistryEntry Maybe
    -> GoguenRegistryEntry Maybe
    -> GoguenRegistryEntry Maybe
combineRegistryEntries new old = GoguenRegistryEntry
    { _goguenRegistryEntry_subject =
        _goguenRegistryEntry_subject new <|> _goguenRegistryEntry_subject old
    , _goguenRegistryEntry_policy =
        _goguenRegistryEntry_policy new <|> _goguenRegistryEntry_policy old
    , _goguenRegistryEntry_name =
        _goguenRegistryEntry_name new `combineAttestedEntry` _goguenRegistryEntry_name old
    , _goguenRegistryEntry_description =
        _goguenRegistryEntry_description new `combineAttestedEntry` _goguenRegistryEntry_description old
    , _goguenRegistryEntry_logo =
        _goguenRegistryEntry_logo new `combineAttestedEntry` _goguenRegistryEntry_logo old
    , _goguenRegistryEntry_url =
        _goguenRegistryEntry_url new `combineAttestedEntry` _goguenRegistryEntry_url old
    , _goguenRegistryEntry_ticker =
        _goguenRegistryEntry_ticker new `combineAttestedEntry` _goguenRegistryEntry_ticker old
    , _goguenRegistryEntry_decimals =
        _goguenRegistryEntry_decimals new `combineAttestedEntry` _goguenRegistryEntry_decimals old
    }
  where
    combineAttestedEntry a b = case (a, b) of
        (Just (Attested sigA nA valA), Just (Attested sigB nB valB)) | valA == valB ->
            Just $ Attested (sigA ++ sigB) (max nA nB) valA
        (Just (Attested sigs nA val), Just (Attested _ nB _)) ->
            Just $ Attested sigs (max nA nB + 1) val
        _ ->
            a <|> b

attestFields
    :: SomeSigningKey
    -> [AttestationField]
    -> PartialGoguenRegistryEntry
    -> Either Text PartialGoguenRegistryEntry
attestFields (SomeSigningKey someSigningKey) props old = do
    subj <- case _goguenRegistryEntry_subject old of
        Just subj -> pure subj
        Nothing -> Left "Cannot attest without a subject record"
    pure $ old
        { _goguenRegistryEntry_name =
            attestField AttestationFieldName subj <$> _goguenRegistryEntry_name old
        , _goguenRegistryEntry_description =
            attestField AttestationFieldDescription subj <$> _goguenRegistryEntry_description old
        , _goguenRegistryEntry_logo =
            attestField AttestationFieldLogo subj <$> _goguenRegistryEntry_logo old
        , _goguenRegistryEntry_url =
            attestField AttestationFieldUrl subj <$> _goguenRegistryEntry_url old
        , _goguenRegistryEntry_ticker =
            attestField AttestationFieldTicker subj <$> _goguenRegistryEntry_ticker old
        , _goguenRegistryEntry_decimals =
            attestField AttestationFieldDecimals subj <$> _goguenRegistryEntry_decimals old
        }
  where
    attestField
        :: WellKnownProperty p
        => AttestationField
        -> Subject
        -> Attested p
        -> Attested p
    attestField fld subj (Attested att n wk) =
        if fld `elem` props
        then Attested attestations n wk
        else Attested att n wk
      where
        wkHash = hashesForAttestation subj wk n
        newAttestationSig = makeAttestationSignature someSigningKey wkHash
        attestations = newAttestationSig:att

handleEntryUpdateArguments :: EntryUpdateArguments -> IO ()
handleEntryUpdateArguments (EntryUpdateArguments fInfo keyfile props newEntryInfo logoM policyM) = do
    attestKey <- mapM readKeyFile keyfile

    record <- case _FileInfoEntryOperation fInfo of
        EntryOperationRevise -> do
            let dfn = draftFilename fInfo
            exists <- doesFileExist $ draftFilename fInfo
            let readFn = if exists then dfn else canonicalFilename fInfo
            json <- Aeson.eitherDecodeFileStrict readFn
            parseJSON (left T.pack json)
        EntryOperationInitialize -> pure $ GoguenRegistryEntry
            { _goguenRegistryEntry_subject = Just (_FileInfoSubject fInfo)
            , _goguenRegistryEntry_policy = Nothing
            , _goguenRegistryEntry_name = Nothing
            , _goguenRegistryEntry_description = Nothing
            , _goguenRegistryEntry_logo = Nothing
            , _goguenRegistryEntry_url = Nothing
            , _goguenRegistryEntry_ticker = Nothing
            , _goguenRegistryEntry_decimals = Nothing
            }

    policy <- case policyM of
        Just filenameOrCBOR -> do
            result <- doesFileExist filenameOrCBOR >>= \case
                True  -> do
                    json <- Aeson.eitherDecodeFileStrict filenameOrCBOR
                    pure $ Aeson.parseEither parseWellKnown =<< json
                False ->
                    pure $ Aeson.parseEither parseWellKnown (Aeson.toJSON filenameOrCBOR)
            fmap Just $ dieOnLeft "Loading policy" $ left T.pack result
        Nothing ->
            pure Nothing

    logo <- case logoM of
        Just fname -> do
            logo <- BS.readFile fname
            let logoAsJSON = Aeson.toJSON $ B8.unpack $ B64.encode logo
            fmap Just $ dieOnLeft "Loading image data" $ left T.pack $
                emptyAttested <$> Aeson.parseEither parseWellKnown logoAsJSON
        Nothing ->
            pure Nothing

    let newRecord = combineRegistryEntries (newEntryInfo
            { _goguenRegistryEntry_logo = logo
            , _goguenRegistryEntry_policy = policy
            }) record

    newRecordWithAttestations <- dieOnLeft "Adding attestation" $ case attestKey of
        Just k -> attestFields k props newRecord
        Nothing -> pure newRecord

    let finalVerificationStatus = validateEntry newRecordWithAttestations

    BL8.writeFile (draftFilename fInfo) (Aeson.encodePretty newRecordWithAttestations)
    case _FileInfoDraftStatus fInfo of
        DraftStatusFinal -> do
            dieOnLeft "Finalizing" finalVerificationStatus
            renameFile (draftFilename fInfo) $ canonicalFilename fInfo
            putStrLn $ canonicalFilename fInfo
        DraftStatusDraft -> do
            putStrLn $ draftFilename fInfo

    exitSuccess
  where
    dieOnLeft :: Text -> Either Text a -> IO a
    dieOnLeft lbl eVal = case eVal of
        Left err  -> die $ lbl <> ": " <> err
        Right val -> pure val

    readKeyFile :: FilePath -> IO SomeSigningKey
    readKeyFile skFname = do
        asNormalKey   <- fmap SomeSigningKey <$>
            readSigningKeyFile AsPaymentKey (SigningKeyFile skFname)
        asExtendedKey <- fmap SomeSigningKey <$>
            readSigningKeyFile AsPaymentExtendedKey (SigningKeyFile skFname)

        dieOnLeft "Error reading key file" $
            left show $ asNormalKey `orElse_` asExtendedKey

      where
        orElse_ a b = either (const b) Right a

    parseJSON :: Either Text Aeson.Value -> IO (PartialGoguenRegistryEntry)
    parseJSON registryJSON = dieOnLeft "Parse error" $ do
        json <- registryJSON
        left T.pack $ Aeson.parseEither parseRegistryEntry json

handleValidate :: Severity -> FilePath -> Maybe FilePath -> IO ()
handleValidate logSeverity fpA mFpB = do
  let
    logAction :: MonadIO m => LogAction m Message
    logAction = filterBySeverity logSeverity msgSeverity (cmap fmtMessage logTextStdout)

  usingLoggerT logAction $ do
    fileA  <- parseFile parseValidationMetadata fpA 
    mFileB <- traverse (parseFile parseValidationMetadata) mFpB

    let
      difference =
        case mFileB of
          Nothing    -> Added fileA
          Just fileB -> Changed fileA fileB

    case walletValidationRules `apply` difference of
      Failure errs -> do
        _ <- traverse (log E . prettyPrintValidationError prettyPrintWalletValidationError) errs
        liftIO exitFailure
      Success ()   -> do
        log I "Wallet metadata validation successful!"
        liftIO exitSuccess

  where
    parseFile :: (MonadIO m, WithLog env Message m) => (Text -> m a) -> FilePath -> m (File a)
    parseFile parserA fp = do
      size     <- liftIO $ withFile fp IO.ReadMode $ hFileSize
      contents <- liftIO $ handleAny (const mempty) $ readFile fp
      a        <- parserA contents
      pure (File a (fromInteger size) fp)

    parseValidationMetadata :: (MonadIO m, WithLog env Message m) => Text -> m Metadata
    parseValidationMetadata contents = do
      case Aeson.eitherDecode (TL.encodeUtf8 $ TL.fromStrict contents) of
        Left err   -> do
          log E $ "Failed to parse validation metadata. Error was: '" <> T.pack (show err) <> "', from JSON: '" <> contents <> "'."
          liftIO $ exitFailure
        Right meta -> pure meta
