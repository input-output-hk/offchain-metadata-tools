module Config where

import           Options.Applicative

import           Cardano.Metadata.Store.Postgres.Config (Opts, parseOpts)

opts :: ParserInfo Opts
opts =
  info
    parseOpts
    ( fullDesc
    <> progDesc "Start the metadata web hook"
    <> header "metadata-webhook - a tool to update off-chain metadata"
    )
