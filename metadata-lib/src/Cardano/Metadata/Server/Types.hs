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

import           Control.Applicative           (some)
import           Control.DeepSeq               (NFData)
import           Control.Monad                 ((>=>))
import           Data.Aeson                    (FromJSON, FromJSONKey, ToJSON,
                                                ToJSONKey, (.:), (.:?))
import qualified Data.Aeson                    as Aeson
import           Data.Aeson.TH
import qualified Data.Aeson.Types              as Aeson
import           Data.ByteArray.Encoding       (Base (Base16, Base64),
                                                convertFromBase, convertToBase)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as BS
import           Data.Functor                  (($>))
import           Data.Functor.Identity         (Identity (Identity))
import qualified Data.HashMap.Strict           as HM
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                    (fromMaybe)
import           Data.Monoid                   (First (First), getFirst)
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.String                   (IsString, fromString)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           GHC.Generics
import           GHC.Show                      (showSpace)
import           Network.URI                   (URI, parseAbsoluteURI,
                                                uriScheme)
import           Numeric.Natural               (Natural)
import           Quiet                         (Quiet (Quiet))
import           Text.Casing
import           Text.ParserCombinators.ReadP  (choice, string)
import           Text.Read                     (Read (readPrec), readEither)
import qualified Text.Read                     as Read (lift)
import           Web.HttpApiData               (FromHttpApiData, ToHttpApiData,
                                                parseUrlPiece, toUrlPiece)

import           Cardano.Metadata.Types.Common (AnnotatedSignature,
                                                PropertyName (PropertyName),
                                                Subject (Subject),
                                                unPropertyName)
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
