{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Metadata.Types
    ( -- * Subject
      Subject (..)
    , hashSubject

      -- * WellKnownProperty / Property
    , WellKnownProperty (..)
    , hashWellKnownProperty
    , Property (..)
    , hashProperty

      -- * Well-known Properties
    , Policy (..)
    , hashPolicy
    , evaluatePolicy
    , prettyPolicy
    , verifyPolicy
    , Name (..)
    , Description (..)
    , Logo (..)
    , Url(..)
    , Ticker(..)
    , Decimals(..)

      -- * Attestation
    , Attested (..)
    , AttestationSignature (..)
    , SequenceNumber (..)
    , parseWithAttestation
    , emptyAttested
    , isAttestedBy
    , verifyAttested

      -- * Signing
    , MakeAttestationSignature(..)
    , SomeSigningKey (..)
    , HashesForAttestation (..)
    , hashesForAttestation
    ) where

import Cardano.Prelude

import Cardano.Api
    ( AsType (AsMaryEra, AsScriptInEra)
    , MaryEra
    , PaymentExtendedKey
    , PaymentKey
    , Script (..)
    , ScriptHash
    , ScriptInEra (..)
    , SigningKey
    , SimpleScript
    , SimpleScriptVersion (..)
    , deserialiseFromCBOR
    , hashScript
    , serialiseToCBOR
    , serialiseToRawBytes
    , serialiseToRawBytesHex
    )
import Cardano.Crypto.DSIGN
    ( DSIGNAlgorithm
    , Ed25519DSIGN
    , SigDSIGN
    , VerKeyDSIGN
    , deriveVerKeyDSIGN
    , rawDeserialiseSigDSIGN
    , rawDeserialiseSignKeyDSIGN
    , rawDeserialiseVerKeyDSIGN
    , rawSerialiseSigDSIGN
    , rawSerialiseVerKeyDSIGN
    , signDSIGN
    , verifyDSIGN
    )
import Cardano.Crypto.Hash
    ( Blake2b_256, Hash, castHash, hashToBytes, hashWith )
import Cardano.Ledger.ShelleyMA.Timelocks
    ( Timelock (RequireAllOf, RequireAnyOf, RequireMOf, RequireSignature, RequireTimeExpire, RequireTimeStart)
    , ValidityInterval (..)
    , ppTimelock
    )
import Codec.Picture.Png
    ( decodePng )
import Control.Category
    ( id )
import Control.Monad.Fail
    ( fail )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), (.:), (.=) )
import Data.Maybe
    ( fromJust )
import Network.URI
    ( URI (..), parseAbsoluteURI )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardCrypto )
import Shelley.Spec.Ledger.BaseTypes
    ( StrictMaybe (..) )
import Shelley.Spec.Ledger.Keys
    ( KeyHash (..), KeyRole (Witness) )

import qualified AesonHelpers
import qualified Cardano.Api as Api
import qualified Cardano.Crypto.Wallet as CC
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B8
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Shelley.Spec.Ledger.Keys as Shelley

--
-- Subject
--

newtype Subject = Subject { unSubject :: Text }
    deriving stock (Eq, Show)
    deriving newtype (ToJSON)

hashSubject :: Subject -> Hash Blake2b_256 Subject
hashSubject = hashWith (CBOR.toStrictByteString . CBOR.encodeString . unSubject)


--
-- WellKnownProperty / Property
--

class WellKnownProperty p where
    wellKnownPropertyName :: f p -> Property
    wellKnownToBytes :: p -> CBOR.Encoding
    wellKnownToJSON :: p -> Aeson.Value
    parseWellKnown :: Aeson.Value -> Aeson.Parser p

hashWellKnownProperty :: WellKnownProperty p => p -> Hash Blake2b_256 Value
hashWellKnownProperty = castHash . hashWith (CBOR.toStrictByteString . wellKnownToBytes)

data Value
newtype Property = Property { unProperty :: Text }
    deriving (Show, Eq, Ord)

hashProperty :: Property -> Hash Blake2b_256 Property
hashProperty = hashWith (CBOR.toStrictByteString . CBOR.encodeString . unProperty)

newtype Decimals = Decimals { unDecimals :: Int }
  deriving (Eq, Show)

instance WellKnownProperty Decimals where
  wellKnownPropertyName _ =
    Property "decimals"
  wellKnownToBytes =
    CBOR.encodeInt . unDecimals
  wellKnownToJSON =
    toJSON . unDecimals
  parseWellKnown =
    parseJSON >=> validateMetadataDecimals

data Policy = Policy
    { rawPolicy :: Text
    , getPolicy :: ScriptInEra MaryEra
    } deriving (Eq, Show)

instance WellKnownProperty Policy where
    wellKnownPropertyName _ =
        Property "policy"
    wellKnownToBytes =
        CBOR.encodeString . rawPolicy
    wellKnownToJSON =
        toJSON . rawPolicy
    parseWellKnown = \v -> parseAsText v <|> parseAsObject v
      where
        parseAsText = Aeson.withText "policy" validateMetadataPolicy
        parseAsObject o = parseJSON o <&> \s -> Policy
            { rawPolicy = T.decodeUtf8 $ B16.encode $ serialiseToCBOR s
            , getPolicy = s
            }

prettyPolicy :: Policy -> Text
prettyPolicy = \case
    Policy _ (ScriptInEra _ (SimpleScript SimpleScriptV1 s)) ->
        show $ ppTimelock $ toAllegraTimelock s
    Policy _ (ScriptInEra _ (SimpleScript SimpleScriptV2 s)) ->
        show $ ppTimelock $ toAllegraTimelock s
#if !(MIN_VERSION_base(4,14,0))
    _ -> panic "impossible pattern match"
#endif

hashPolicy :: Policy -> ScriptHash
hashPolicy (Policy _ (ScriptInEra _ script)) =
    hashScript script

verifyPolicy
    :: Policy
    -> Subject
    -> Either Text ()
verifyPolicy policy (Subject subject) = do
    let policyId = T.pack . B8.unpack . serialiseToRawBytesHex . hashPolicy $ policy
    unless (policyId `T.isPrefixOf` subject) $ Left $ unlines
        [ "The policy should re-hash to the first 28 bytes of the subject."
        , "Expected: " <> T.take 56 subject
        , "Got: " <> policyId
        ]

evaluatePolicy
    :: Policy
    -> [AttestationSignature]
    -> Either Text ()
evaluatePolicy (Policy _ script) sigs =
    case script of
        ScriptInEra _ (SimpleScript SimpleScriptV1 s) ->
            evaluateScript $ toAllegraTimelock s
        ScriptInEra _ (SimpleScript SimpleScriptV2 s) ->
            evaluateScript $ toAllegraTimelock s
#if !(MIN_VERSION_base(4,14,0))
        _ -> panic "impossible pattern match"
#endif
  where
    evaluateScript :: Timelock StandardCrypto -> Either Text ()
    evaluateScript s
        | isValidScript hashes (ValidityInterval SNothing SNothing) s =
            Right ()
        | otherwise =
            Left "Unable to validate the monetary policy now and with current attestations."

    hashes :: Set (KeyHash 'Witness StandardCrypto)
    hashes = Set.fromList
        $ Shelley.hashKey . Shelley.VKey . _attestationSignature_publicKey <$> sigs

    isValidScript
        :: (crypto ~ StandardCrypto)
        => Set (KeyHash 'Witness crypto)
        -> ValidityInterval
        -> Timelock crypto
        -> Bool
    isValidScript _vhks (ValidityInterval _start _) (RequireTimeStart _lockStart) =
        True
    isValidScript _vhks (ValidityInterval _ _end) (RequireTimeExpire _lockExp) =
        True
    isValidScript vhks _vi (RequireSignature hash) =
        Set.member hash vhks
    isValidScript vhks vi (RequireAllOf xs) =
        all (isValidScript vhks vi) xs
    isValidScript vhks vi (RequireAnyOf xs) =
        any (isValidScript vhks vi) xs
    isValidScript vhks vi (RequireMOf m xs) =
        m <= sum (fmap (\x -> if isValidScript vhks vi x then 1 else 0) xs)

-- | Conversion for the 'Timelock.Timelock' language that is shared between the
-- Allegra and Mary eras.
--
toAllegraTimelock :: forall lang. SimpleScript lang -> Timelock StandardCrypto
toAllegraTimelock = go
  where
    go :: SimpleScript lang -> Timelock StandardCrypto
    go (Api.RequireSignature (Api.PaymentKeyHash kh)) = RequireSignature (Shelley.coerceKeyRole kh)
    go (Api.RequireAllOf s) = RequireAllOf (Seq.fromList (map go s))
    go (Api.RequireAnyOf s) = RequireAnyOf (Seq.fromList (map go s))
    go (Api.RequireMOf m s) = RequireMOf m (Seq.fromList (map go s))
    go (Api.RequireTimeBefore _ t) = RequireTimeExpire t
    go (Api.RequireTimeAfter  _ t) = RequireTimeStart  t

-- | "name" is a well-known property whose value must be a string
newtype Name = Name { unName :: Text }
    deriving (Show, Eq)

instance WellKnownProperty Name where
    wellKnownPropertyName _ =
        Property "name"
    wellKnownToBytes =
        CBOR.encodeString . unName
    wellKnownToJSON =
        toJSON . unName
    parseWellKnown =
        Aeson.withText "name" validateMetadataName

-- | "description" is a well-known property whose value must be a string
newtype Description = Description { unDescription :: Text }
    deriving stock (Show, Eq)
    deriving newtype (ToJSON)

instance WellKnownProperty Description where
    wellKnownPropertyName _ =
        Property "description"
    wellKnownToBytes =
        CBOR.encodeString . unDescription
    wellKnownToJSON =
        toJSON . unDescription
    parseWellKnown =
        Aeson.withText "description" validateMetadataDescription

newtype Logo = Logo { unLogo :: ByteString }
    deriving (Show, Eq)

instance WellKnownProperty Logo where
    wellKnownPropertyName _ =
        Property "logo"
    wellKnownToBytes =
        CBOR.encodeBytes . unLogo
    wellKnownToJSON =
        toJSON . B8.unpack . B64.encode . unLogo
    parseWellKnown =
        Aeson.withText "logo" (either fail validateMetadataLogo . B64.decode . T.encodeUtf8)

newtype Url = Url { unUrl :: URI }
    deriving (Show, Eq)

instance WellKnownProperty Url where
    wellKnownPropertyName _ =
        Property "url"
    wellKnownToBytes =
        CBOR.encodeString . show . unUrl
    wellKnownToJSON =
        toJSON . show @_ @Text . unUrl
    parseWellKnown =
        Aeson.withText "url" validateMetadataURL

newtype Ticker = Ticker { unTicker :: Text }
    deriving (Show, Eq)

instance WellKnownProperty Ticker where
    wellKnownPropertyName _ =
        Property "ticker"
    wellKnownToBytes =
        CBOR.encodeString . unTicker
    wellKnownToJSON =
        toJSON . unTicker
    parseWellKnown =
        Aeson.withText "ticker" validateMetadataTicker

--
-- Validators
--

validateMinLength :: MonadFail f => Int -> Text -> f Text
validateMinLength n text
    | len >= n = pure text
    | otherwise = fail $ "Length must be at least " ++ show n ++ " characters, got " ++ show len
  where
    len = T.length text

validateMaxLength :: MonadFail f => Int -> Text -> f Text
validateMaxLength n text
    | len <= n = pure text
    | otherwise = fail $ "Length must be no more than " ++ show n ++ " characters, got " ++ show len
  where
    len = T.length text

validateBase16 :: MonadFail f => Text -> f ByteString
validateBase16 =
    either fail pure . B16.decode . T.encodeUtf8

validateMetadataName :: MonadFail f => Text -> f Name
validateMetadataName = fmap Name .
    (validateMinLength 1 >=> validateMaxLength 50)

validateScriptInEra :: MonadFail f => ByteString -> f (ScriptInEra MaryEra)
validateScriptInEra =
    either (fail . show) pure . deserialiseFromCBOR (AsScriptInEra AsMaryEra)

validateMetadataPolicy :: MonadFail f => Text -> f Policy
validateMetadataPolicy t = Policy t <$>
    (validateBase16 >=> validateScriptInEra) t

validateMetadataTicker :: MonadFail f => Text -> f Ticker
validateMetadataTicker = fmap Ticker .
    (validateMinLength 2 >=> validateMaxLength 5)

validateMetadataDescription :: MonadFail f => Text -> f Description
validateMetadataDescription = fmap Description .
    validateMaxLength 500

validateMetadataDecimals :: MonadFail f => Int -> f Decimals
validateMetadataDecimals i
  | i >= 0 && i <= 255 = pure $ Decimals i
  | otherwise          = fail $ "Decimal value must be in the range [0, 255] (inclusive)"

validateMetadataLogo :: MonadFail f => ByteString -> f Logo
validateMetadataLogo bytes
    | len <= maxLen =
        case decodePng bytes of
            Left e     -> fail $ "Verifying PNG: " <> e
            Right _png -> pure (Logo bytes)
    | otherwise = fail $ "Length must be no more than " ++ show maxLen ++ " bytes, got " ++ show len
  where
    len = BS.length bytes
    maxLen = 65536

validateMetadataURL :: MonadFail f => Text -> f Url
validateMetadataURL = fmap Url .
    (validateMaxLength 250 >=> validateURI >=> validateHttps)
  where
      validateURI = maybe (fail "Not an absolute URI") pure
          . parseAbsoluteURI
          . T.unpack
      validateHttps u@(uriScheme -> scheme)
          | scheme == "https:" = pure u
          | otherwise = fail $ "Scheme must be https: but got " ++ scheme

--
-- Attesting
--

-- | Metadata entries can be provided along with annotated signatures
-- attesting to the validity of those entry values.
data Attested a = Attested
    { _attested_signatures :: [AttestationSignature]
    , _attested_sequence_number :: SequenceNumber
    , _attested_property :: a
    } deriving (Eq, Functor, Show)

instance ToJSON a => ToJSON (Attested a) where
    toJSON a = Aeson.object
        [ "value" .= _attested_property a
        , "sequenceNumber" .= _attested_sequence_number a
        , "signatures" .= _attested_signatures a
        ]

emptyAttested
    :: a
    -> Attested a
emptyAttested a = Attested
    { _attested_signatures = []
    , _attested_sequence_number = 0
    , _attested_property = a
    }

isAttestedBy
    :: HashesForAttestation
    -> AttestationSignature
    -> Either Text ()
isAttestedBy hashes sig = first T.pack $ verifyDSIGN ()
    (_attestationSignature_publicKey sig)
    (hashToBytes (attestationDigest hashes))
    (_attestationSignature_signature sig)

verifyAttested
    :: Attested HashesForAttestation
    -> Either [AttestationSignature] ()
verifyAttested attested =
    let
        (invalids, _) = partitionEithers $ flip fmap (_attested_signatures attested) $ \sig ->
            first (const sig) $ isAttestedBy (_attested_property attested) sig
    in
        case invalids of
            [] -> Right ()
            _ -> Left invalids

parseWithAttestation
    :: WellKnownProperty p
    => Aeson.Value
    -> Aeson.Parser (Attested p)
parseWithAttestation = Aeson.withObject "property with attestation" $ \o -> do
    value <- parseWellKnown =<< o .: "value"
    attestations <- (o .: "signatures" >>=) $ Aeson.withArray "Annotated Signatures" $
        fmap toList . mapM (Aeson.withObject "Attestation" (parseAnnotatedSignature AttestationSignature))
    sequenceNumber <- SequenceNumber <$> o .: "sequenceNumber"
    pure $ Attested
        { _attested_signatures = attestations
        , _attested_property = value
        , _attested_sequence_number = sequenceNumber
        }

-- | An 'AttestationSignature is a pair of a public key and a signature
-- that can be verified with that public key of a message derived from
-- the subject, property name, and property value being attested to.
--
-- In particular, the message is hash(hash(subject) + hash (property_name) + hash(property_value))
data AttestationSignature = AttestationSignature
    { _attestationSignature_publicKey :: VerKeyDSIGN Ed25519DSIGN
    , _attestationSignature_signature :: SigDSIGN Ed25519DSIGN
    } deriving (Eq, Show)

instance ToJSON AttestationSignature where
    toJSON a = Aeson.object
        [ "publicKey" .=
            B8.unpack (B16.encode $ rawSerialiseVerKeyDSIGN (_attestationSignature_publicKey a))
        , "signature" .=
            B8.unpack (B16.encode $ rawSerialiseSigDSIGN (_attestationSignature_signature a))
        ]

parseAnnotatedSignature
    :: DSIGNAlgorithm v
    => (VerKeyDSIGN v -> SigDSIGN v -> x)
    -> Aeson.Object
    -> Aeson.Parser x
parseAnnotatedSignature f o = do
    publicKeyField <- o .: "publicKey"
    signatureField <- o .: "signature"
    publicKey <- flip (Aeson.withText "publicKey") publicKeyField $ \t ->
        maybe (fail $ T.unpack $ "Couldn't parse verification key: " <> t) pure $
            rawDeserialiseVerKeyDSIGN =<< eitherToMaybe (B16.decode $ T.encodeUtf8 t)
    signature <- flip (Aeson.withText "signature") signatureField $ \t ->
        maybe (fail $ T.unpack $ "Couldn't parse signature " <> t) pure $
            rawDeserialiseSigDSIGN =<< eitherToMaybe (B16.decode $ T.encodeUtf8 t)
    AesonHelpers.noOtherFields "annotated signature" o ["publicKey", "signature"]
    pure $ f publicKey signature
  where
    eitherToMaybe :: Either a b -> Maybe b
    eitherToMaybe = either (const Nothing) Just

newtype SequenceNumber = SequenceNumber Int
    deriving stock (Eq, Show, Read, Ord)
    deriving newtype (Num, Enum, Real, Integral, ToJSON)

hashSequenceNumber
    :: SequenceNumber
    -> Hash Blake2b_256 SequenceNumber
hashSequenceNumber =
    hashWith (CBOR.toStrictByteString . CBOR.encodeWord . fromIntegral)


--
-- Signing
--

data SomeSigningKey where
    SomeSigningKey
        :: forall keyrole. (MakeAttestationSignature keyrole, Show (SigningKey keyrole))
        => SigningKey keyrole
        -> SomeSigningKey

deriving instance Show (SomeSigningKey)

-- | Hashes required to produce a message for attestation purposes
data HashesForAttestation = HashesForAttestation
    { _hashesForAttestation_subject :: Hash Blake2b_256 Subject
    , _hashesForAttestation_property :: Hash Blake2b_256 Property
    , _hashesForAttestation_value :: Hash Blake2b_256 Value
    , _hashesForAttestation_sequence_number :: Hash Blake2b_256 SequenceNumber
    }

class MakeAttestationSignature keyrole where
    makeAttestationSignature
        :: SigningKey keyrole
        -> HashesForAttestation
        -> AttestationSignature

instance MakeAttestationSignature PaymentKey where
    makeAttestationSignature key hashes =
        AttestationSignature
            { _attestationSignature_publicKey =
                deriveVerKeyDSIGN prv
            , _attestationSignature_signature = signDSIGN ()
                (hashToBytes $ attestationDigest hashes)
                prv
            }
      where
        -- Very ugly cast of a 'PaymentKey' into a SignKeyDSign
        Just prv = rawDeserialiseSignKeyDSIGN (serialiseToRawBytes key)

instance MakeAttestationSignature PaymentExtendedKey where
    makeAttestationSignature key hashes =
        AttestationSignature
            { _attestationSignature_publicKey = unsafeToVerKeyDSign
                $ CC.toXPub xprv
            , _attestationSignature_signature = unsafeToSigDSign
                $ CC.sign (mempty :: ByteString) xprv (hashToBytes $ attestationDigest hashes)
            }
      where
        -- Very ugly cast of a 'PaymentExtendedKey' into an 'XPrv'
        Right xprv = CC.xprv (serialiseToRawBytes key)
        -- NOTE: We can 'safely' cast to VerKeyDSIGN and SigDSIGN for
        -- verification because the signature verification algorithm is the
        -- same for extended and normal keys.
        unsafeToVerKeyDSign = fromJust . rawDeserialiseVerKeyDSIGN . CC.xpubPublicKey
        unsafeToSigDSign = fromJust . rawDeserialiseSigDSIGN . CC.unXSignature

hashesForAttestation
   :: forall p. WellKnownProperty p
   => Subject
   -> p
   -> SequenceNumber
   -> HashesForAttestation
hashesForAttestation s w n = HashesForAttestation
    { _hashesForAttestation_subject =
        hashSubject s
    , _hashesForAttestation_property =
        hashProperty (wellKnownPropertyName (Proxy @p))
    , _hashesForAttestation_value =
        hashWellKnownProperty w
    , _hashesForAttestation_sequence_number =
        hashSequenceNumber n
    }

attestationDigest
    :: HashesForAttestation
    -> Hash Blake2b_256 HashesForAttestation
attestationDigest hashes = castHash $ hashWith id $ mconcat
    [ hashToBytes $ _hashesForAttestation_subject hashes
    , hashToBytes $ _hashesForAttestation_property hashes
    , hashToBytes $ _hashesForAttestation_value hashes
    , hashToBytes $ _hashesForAttestation_sequence_number hashes
    ]
