module Config where

import Options.Applicative

import Cardano.Metadata.Store.File.Config
    ( Opts, parseOpts )

opts :: ParserInfo Opts
opts =
  info
    ( parseOpts )
    ( fullDesc
    <> progDesc "Start the metadata web server"
    <> header "metadata-server - a tool to serve off-chain metadata"
    )
