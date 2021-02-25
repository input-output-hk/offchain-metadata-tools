{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Metadata.Server.API where

import           Data.Text                     (Text)
import           Servant.API

import           Cardano.Metadata.Server.Types (BatchRequest, BatchResponse)
import           Cardano.Metadata.Types.Common (PropertyName, Subject)
import qualified Cardano.Metadata.Types.Weakly as Weakly

type MetadataServerAPI =
  "metadata" :>
    ( Capture "subject" Subject                                                      :> Get '[JSON] Weakly.Metadata
 :<|> Capture "subject" Subject :> "properties"                                      :> Get '[JSON] Weakly.Metadata
 :<|> Capture "subject" Subject :> "properties" :> Capture "properties" PropertyName :> Get '[JSON] Weakly.Metadata
 :<|> "query" :> ReqBody '[JSON] BatchRequest :> Post '[JSON] BatchResponse
    )
