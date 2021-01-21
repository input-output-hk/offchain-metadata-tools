
module Metadata.Server.API where

import Metadata.Server.Types

type MetadataServerAPI =
  "metadata" :>
    ( Capture "subject" Subject                                                    :> Get '[JSON] Entry
 :<|> Capture "subject" Subject :> "properties"                                    :> Get '[JSON] Entry
 :<|> Capture "subject" Subject :> "properties" :> Capture "properties" Text :> Get '[JSON] AnyProperty
 :<|> "query" :> ReqBody '[JSON] BatchRequest :> Post '[JSON] BatchResponse
    )
