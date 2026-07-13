{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Golden tests against real entries from the mainnet
-- cardano-token-registry (see test/fixtures). These pin down the
-- policy wire format ([tag, script] CBOR, hex-encoded) and the
-- attestation signature scheme: both must remain byte-for-byte
-- compatible with the data already in the wild.
import Cardano.Prelude

import Data.Aeson ( (.:) )
import System.FilePath ( takeExtension, (</>) )

import Cardano.Metadata.GoguenRegistry
    ( GoguenRegistryEntry (..), parseRegistryEntry, validateEntry )
import Cardano.Metadata.Types
    ( Policy (..), serialiseScriptToCBOR )

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Directory ( listDirectory )
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
    let fixtureDir = "test/fixtures"
    fixtureFiles <- sort . filter ((== ".json") . takeExtension) <$> listDirectory fixtureDir
    when (null fixtureFiles) $
        panic "no fixtures found in test/fixtures"
    fixtures <- forM fixtureFiles $ \f -> do
        contents <- Aeson.eitherDecodeFileStrict (fixtureDir </> f)
        either (panic . T.pack) (pure . (,) f) contents
    defaultMain $ testGroup "golden registry entries"
        [ testGroup f
            [ testCase "entry parses and validates" (validates json)
            , testCase "policy re-serialises byte-for-byte" (policyRoundtrips json)
            ]
        | (f, json) <- fixtures
        ]

-- | The entry must parse and pass full validation: policy CBOR
-- decoding, script re-hashing against the subject, and verification of
-- the (real) attestation signatures.
validates :: Aeson.Value -> Assertion
validates json = do
    entry <- either assertFailureStr pure $
        Aeson.parseEither parseRegistryEntry json
    either (assertFailureStr . T.unpack) pure $
        validateEntry entry

-- | Decoding the policy and re-serialising it must reproduce the
-- original bytes exactly.
policyRoundtrips :: Aeson.Value -> Assertion
policyRoundtrips json = do
    rawHex :: Text <- either assertFailureStr pure $
        Aeson.parseEither (Aeson.withObject "entry" (.: "policy")) json
    entry <- either assertFailureStr pure $
        Aeson.parseEither parseRegistryEntry json
    policy <- maybe (assertFailureStr "fixture has no policy") pure $
        _goguenRegistryEntry_policy entry
    let reencoded = T.decodeUtf8 . B16.encode . serialiseScriptToCBOR . getPolicy $ policy
    reencoded @?= rawHex

assertFailureStr :: [Char] -> IO a
assertFailureStr = assertFailure
