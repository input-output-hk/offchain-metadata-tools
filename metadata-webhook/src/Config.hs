module Config where

import Data.Text ( Text )
import qualified Data.Text as T
import Options.Applicative

import Cardano.Metadata.Store.Postgres.Config ( Opts, parseOpts )

-- | optGithubOwner/optGithubRepo pin the single GitHub repository this
-- webhook is willing to fetch file contents from. They're used to build the
-- GitHub API URL ourselves and to reject push events for any other
-- repository, rather than trusting a URL supplied by the webhook payload
-- (which would let a forged/malicious payload redirect our GitHub token to
-- an arbitrary host).
data WebhookOpts = WebhookOpts
  { optGithubOwner :: Text
  , optGithubRepo  :: Text
  , optDbOpts      :: Opts
  }

parseWebhookOpts :: Parser WebhookOpts
parseWebhookOpts = WebhookOpts
  <$> (T.pack <$> strOption (long "github-owner" <> metavar "OWNER" <> help "Owner (user or organization) of the sole GitHub repository this webhook will fetch file contents from"))
  <*> (T.pack <$> strOption (long "github-repo" <> metavar "REPO" <> help "Name of the sole GitHub repository this webhook will fetch file contents from"))
  <*> parseOpts

opts :: ParserInfo WebhookOpts
opts =
  info
    parseWebhookOpts
    ( fullDesc
    <> progDesc "Start the metadata web hook"
    <> header "metadata-webhook - a tool to update off-chain metadata"
    )
