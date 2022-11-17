{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Metadata.Validation.Types
  ( Difference (..)
  , ValidationFn
  , Metadata(Metadata)
  , metaSubject
  , metaAttestedProperties
  , metaVerifiableProperties
  , onMatchingAttestedProperties
  , mkValidation
  , invalid
  , valid
  ) where

import Data.Aeson ( FromJSON, ToJSON, (.:) )
import qualified Data.Aeson as Aeson
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Aeson.KeyMap as KM
import Data.List.NonEmpty ( NonEmpty )
import qualified Data.Map.Merge.Strict as M
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.Validation ( Validation (Failure) )

import Cardano.Metadata.Transform
import Cardano.Metadata.Types.Common
    ( Property
    , PropertyName
    , PropertyType (Attested, Verifiable)
    , Subject
    , fromPropertyNameList
    , toPropertyNameList
    )

data Metadata
  = Metadata { metaSubject              :: Subject
             , metaAttestedProperties   :: Map PropertyName (Property 'Attested Aeson.Value)
             , metaVerifiableProperties :: Map PropertyName (Property 'Verifiable Aeson.Value)
             }
  deriving (Eq, Show)

metaProperties :: Metadata -> Map PropertyName Aeson.Value
metaProperties (Metadata _ pAttested pVerifiable) =
  (Aeson.toJSON <$> pAttested) <> (Aeson.toJSON <$> pVerifiable)

-- | Run the given function on each set of attested properties with
-- matching names in the two sets of metadata.
onMatchingAttestedProperties
  :: Monoid m
  => Metadata
  -> Metadata
  -> (Property 'Attested Aeson.Value -> Property 'Attested Aeson.Value -> m)
  -> m
onMatchingAttestedProperties m1 m2 f =
  let
    attested1 = metaAttestedProperties m1
    attested2 = metaAttestedProperties m2

    takeBoth      = M.zipWithAMatched      (\_key a b           -> pure (a,b))
    ignoreMissing = M.traverseMaybeMissing (\_key _missingValue -> pure Nothing)

    matchingOnly = M.merge ignoreMissing ignoreMissing takeBoth attested1 attested2
  in
    foldMap (\(_k, v) -> (uncurry f) v) $ M.toList matchingOnly

parseProperties
  :: Aeson.Object
  -> ( Map PropertyName (Property 'Attested Aeson.Value)
     , Map PropertyName (Property 'Verifiable Aeson.Value)
     )
parseProperties obj =
  let
    properties :: [(PropertyName, Aeson.Value)]
    properties = toPropertyNameList $ KM.toList $ foldr KM.delete obj ["subject"]

    partitioned =
      foldr (\(name, val) (atts,vers) ->
        case Aeson.fromJSON val :: Aeson.Result (Property 'Attested Aeson.Value) of
          Aeson.Success attested -> ((name, attested):atts,             vers)
          Aeson.Error _          -> (                 atts, (name, val):vers)
      ) mempty properties
  in
    Bifunctor.bimap M.fromList M.fromList partitioned

data Difference a
  = Added a
  | Changed a a
  | Removed a
  deriving (Eq, Show)

type ValidationFn a e = Transform (Difference a) (Validation (NonEmpty e)) ()

mkValidation :: (Difference a -> Validation (NonEmpty e) ()) -> ValidationFn a e
mkValidation = mkTransform

invalid :: e -> Validation (NonEmpty e) a
invalid = Failure . pure

valid :: Validation (NonEmpty e) ()
valid = pure ()

-- Instances

instance ToJSON Metadata where
  toJSON meta@(Metadata subject _ _) = Aeson.Object $ KM.fromList $
    [ ("subject", Aeson.toJSON subject)
    ]
    <> (fmap (Aeson.toJSON) <$> (fromPropertyNameList $ M.toList $ metaProperties meta))

instance FromJSON Metadata where
  parseJSON = Aeson.withObject "Weakly-typed Metadata" $ \obj ->
    let
      props = parseProperties obj
    in
      Metadata
      <$> obj .: "subject"
      <*> (pure $ fst props)
      <*> (pure $ snd props)
