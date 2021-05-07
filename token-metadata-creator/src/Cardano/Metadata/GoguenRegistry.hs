{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Metadata.GoguenRegistry where

import Cardano.Prelude

import Cardano.Metadata.Types
    ( Attested (..)
    , Decimals (..)
    , Description (..)
    , Logo (..)
    , Logo (..)
    , Name (..)
    , Policy (..)
    , Property (..)
    , Subject (..)
    , Ticker (..)
    , Url (..)
    , WellKnownProperty (..)
    , evaluatePolicy
    , hashesForAttestation
    , parseWithAttestation
    , prettyPolicy
    , verifyAttested
    , verifyPolicy
    )
import Control.Arrow
    ( left )
import Data.Aeson
    ( ToJSON (..), (.:?), (.=) )

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

-- | The goguen-metadata-registry as maintained by Cardano Foundation
-- is a metadata server that expects entries to have these fields.
-- It is parameterized so that we can handle partially constructed entries.
--
-- Neither the policy nor The subject are attested.
-- Attestations attest to the relationship between the subject and the field
-- and thus already include the subject in the hash. The policy itself is used
-- to verify attestations.
data GoguenRegistryEntry f = GoguenRegistryEntry
    { _goguenRegistryEntry_subject :: f Subject
    , _goguenRegistryEntry_policy :: f Policy
    , _goguenRegistryEntry_name :: f (Attested Name)
    , _goguenRegistryEntry_description :: f (Attested Description)
    , _goguenRegistryEntry_logo :: f (Attested Logo)
    , _goguenRegistryEntry_url :: f (Attested Url)
    , _goguenRegistryEntry_ticker :: f (Attested Ticker)
    , _goguenRegistryEntry_decimals :: f (Attested Decimals)
    }

deriving instance
    ( Show (f Subject)
    , Show (f Policy)
    , Show (f (Attested Name))
    , Show (f (Attested Description))
    , Show (f (Attested Logo))
    , Show (f (Attested Url))
    , Show (f (Attested Ticker))
    , Show (f (Attested Decimals))
    ) => Show (GoguenRegistryEntry f)

deriving instance
    ( Eq (f Subject)
    , Eq (f Policy)
    , Eq (f (Attested Name))
    , Eq (f (Attested Description))
    , Eq (f (Attested Logo))
    , Eq (f (Attested Url))
    , Eq (f (Attested Ticker))
    , Eq (f (Attested Decimals))
    ) => Eq (GoguenRegistryEntry f)

instance ToJSON (GoguenRegistryEntry Maybe) where
    toJSON r = Aeson.object $ mconcat
        [ [ "subject" .= _goguenRegistryEntry_subject r
          , unProperty (wellKnownPropertyName (Proxy @Policy)) .=
              (wellKnownToJSON <$> (_goguenRegistryEntry_policy r))
          , unProperty (wellKnownPropertyName (Proxy @Name)) .=
              (fmap wellKnownToJSON <$> (_goguenRegistryEntry_name r))
          , unProperty (wellKnownPropertyName (Proxy @Description)) .=
              (fmap wellKnownToJSON <$> (_goguenRegistryEntry_description r))
          ]
        , catMaybes
          [ (\x -> unProperty (wellKnownPropertyName (Proxy @Logo)) .= fmap wellKnownToJSON x)
                <$> (_goguenRegistryEntry_logo r)
          , (\x -> unProperty (wellKnownPropertyName (Proxy @Url)) .= fmap wellKnownToJSON x)
                <$> (_goguenRegistryEntry_url r)
          , (\x -> unProperty (wellKnownPropertyName (Proxy @Ticker)) .= fmap wellKnownToJSON x)
                <$> (_goguenRegistryEntry_ticker r)
          , (\x -> unProperty (wellKnownPropertyName (Proxy @Decimals)) .= fmap wellKnownToJSON x)
                <$> (_goguenRegistryEntry_decimals r)
          ]
        ]

type PartialGoguenRegistryEntry = GoguenRegistryEntry Maybe

parseRegistryEntry
    :: Aeson.Value
    -> Aeson.Parser PartialGoguenRegistryEntry
parseRegistryEntry = Aeson.withObject "GoguenRegistryEntry" $ \o -> do
    subject <- o .:? "subject"

    policyRaw <- o .:? unProperty (wellKnownPropertyName $ Proxy @Policy)
    policy <- mapM parseWellKnown policyRaw

    nameField     <- o .:? unProperty (wellKnownPropertyName $ Proxy @Name)
    descField     <- o .:? unProperty (wellKnownPropertyName $ Proxy @Description)
    logoField     <- o .:? unProperty (wellKnownPropertyName $ Proxy @Logo)
    urlField      <- o .:? unProperty (wellKnownPropertyName $ Proxy @Url)
    tickerField   <- o .:? unProperty (wellKnownPropertyName $ Proxy @Ticker)
    decimalsField <- o .:? unProperty (wellKnownPropertyName $ Proxy @Decimals)

    nameAnn   <- mapM parseWithAttestation nameField
    descAnn   <- mapM parseWithAttestation descField
    logoAnn   <- mapM parseWithAttestation logoField
    urlAnn    <- mapM parseWithAttestation urlField
    tickerAnn <- mapM parseWithAttestation tickerField
    decimalsAnn <- mapM parseWithAttestation decimalsField

    pure $ GoguenRegistryEntry
        { _goguenRegistryEntry_subject = Subject <$> subject
        , _goguenRegistryEntry_policy  = policy
        , _goguenRegistryEntry_name = nameAnn
        , _goguenRegistryEntry_description = descAnn
        , _goguenRegistryEntry_logo = logoAnn
        , _goguenRegistryEntry_url = urlAnn
        , _goguenRegistryEntry_ticker = tickerAnn
        , _goguenRegistryEntry_decimals = decimalsAnn
        }

validateEntry
    :: PartialGoguenRegistryEntry
    -> Either Text ()
validateEntry record = do
    -- 1. Verify that mandatory fields are present
    subject <- verifyField _goguenRegistryEntry_subject
    policy  <- verifyField _goguenRegistryEntry_policy
    name    <- verifyField _goguenRegistryEntry_name
    desc    <- verifyField _goguenRegistryEntry_description

    -- 2. Policy should re-hash to first bytes of the subject
    verifyPolicy policy subject

    -- 3. Verify that all attestations have matching signatures
    let verifyLocalAttestations :: WellKnownProperty p => Text -> Attested p -> Either Text ()
        verifyLocalAttestations fieldName (Attested attestations n w) = do
            let hashes = hashesForAttestation subject w n
            left (const $ "attestation verification failed for: " <> fieldName) $ do
                verifyAttested $ Attested attestations n hashes
            let policyEvaluationFailed = unlines
                    [ "policy evaluation failed for: " <> fieldName
                    , "Policy is:"
                    , prettyPolicy policy
                    ]
            left (const policyEvaluationFailed) $
                evaluatePolicy policy attestations

    verifyLocalAttestations "name" name
    verifyLocalAttestations "description" desc

    forM_ (_goguenRegistryEntry_logo record) $ verifyLocalAttestations "logo"
    forM_ (_goguenRegistryEntry_url record) $ verifyLocalAttestations "url"
    forM_ (_goguenRegistryEntry_ticker record) $ verifyLocalAttestations "ticker"
    forM_ (_goguenRegistryEntry_decimals record) $ verifyLocalAttestations "decimals"
  where
    verifyField :: (PartialGoguenRegistryEntry -> Maybe a) -> Either Text a
    verifyField field = maybe (Left missingFields) Right (field record)

    missingFields :: Text
    missingFields = mconcat
        [ missingField "Missing field subject"
            _goguenRegistryEntry_subject
        , missingField "Missing field policy: Use -p to specify"
            _goguenRegistryEntry_policy
        , missingField "Missing field name: Use -n to specify"
            _goguenRegistryEntry_name
        , missingField "Missing field description: Use -d to specify"
            _goguenRegistryEntry_description
        ]

    missingField :: Text -> (PartialGoguenRegistryEntry -> Maybe b) -> Text
    missingField str fld = case fld record of
        Just _  -> ""
        Nothing -> "\n" <> str
