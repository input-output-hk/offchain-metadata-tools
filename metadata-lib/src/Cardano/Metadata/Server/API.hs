{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Metadata.Server.API where

import Servant.API
import Data.Text (Text)

import Cardano.Metadata.Server.Types

type MetadataServerAPI =
  "metadata" :>
    ( Capture "subject" Subject                                              :> Get '[JSON] Entry
 :<|> Capture "subject" Subject :> "properties"                              :> Get '[JSON] Entry
 :<|> Capture "subject" Subject :> "properties" :> Capture "properties" Text :> Get '[JSON] PartialEntry
 :<|> "query" :> ReqBody '[JSON] BatchRequest :> Post '[JSON] BatchResponse
    )
