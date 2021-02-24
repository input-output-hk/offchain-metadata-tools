{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Metadata.Server.Types where

import           Data.Aeson                    (FromJSON, ToJSON, (.:), (.:?))
import qualified Data.Aeson                    as Aeson
import           Data.Aeson.TH
import qualified Data.HashMap.Strict           as HM
import           Text.Casing

import           Cardano.Metadata.Types.Common (PropertyName,
                                                Subject)
import qualified Cardano.Metadata.Types.Weakly as Weakly

-- | Represents the content of a batch request to the metadata system.
--
-- For example, @BatchRequest ["a", "b"] ["preimage", "name"]@ would
-- represent a request for the "preimage" and "name" properties of both
-- subject "a" and "b".
data BatchRequest
  = BatchRequest { bReqSubjects   :: [Subject]
                 , bReqProperties :: Maybe [PropertyName]
                 }
  deriving (Eq, Show)

instance ToJSON BatchRequest where
  toJSON (BatchRequest subjs propNames) = Aeson.Object . HM.fromList $
    [ ("subjects", Aeson.toJSON subjs)
    , ("properties", Aeson.toJSON propNames)
    ]

instance FromJSON BatchRequest where
  parseJSON = Aeson.withObject "BatchRequest" $ \obj ->
    BatchRequest
      <$> obj .:  "subjects"
      <*> obj .:? "properties"

-- | Represents the response of a batch request.
data BatchResponse
  = BatchResponse { bRespSubjects :: [Weakly.Metadata] }
  deriving (Eq, Show)

instance Semigroup BatchResponse where
  (BatchResponse xs) <> (BatchResponse ys) = BatchResponse $ xs <> ys

instance Monoid BatchResponse where
  mempty = BatchResponse mempty

-- Instances

$(deriveJSON defaultOptions{ Aeson.fieldLabelModifier = toCamel . fromHumps . drop 5 } ''BatchResponse)
