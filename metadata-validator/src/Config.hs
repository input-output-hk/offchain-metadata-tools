module Config where

import Options.Applicative
import qualified Data.ByteString     as BS
import Data.Text (Text)
import qualified GitHub as GitHub
import qualified GitHub.Data.Name as GitHub

data AuthScheme = NoAuthScheme
                | OAuthScheme BS.ByteString

data Opts
  = Opts { optAuth        :: AuthScheme
         , optRepoOwner   :: Text
         , optRepoName    :: Text
         , optIssueNumber :: Int
         }

data Config
  = Config { cfgAuth        :: AuthScheme
           , cfgRepoOwner   :: GitHub.Name GitHub.Owner
           , cfgRepoName    :: GitHub.Name GitHub.Repo
           , cfgIssueNumber :: GitHub.IssueNumber
           }

mkConfig :: Opts -> Config
mkConfig (Opts auth owner name issue) =
  Config auth (GitHub.N owner) (GitHub.N name) (GitHub.IssueNumber issue)

opts :: ParserInfo Opts
opts =
  info
    parseOpts
    ( fullDesc
    <> progDesc "Validate a pull request against a GitHub repository."
    <> header "metadata-validator - a tool to validate a metadata entry submitted via pull request."
    )

parseOpts :: Parser Opts
parseOpts = Opts
 <$> pAuthScheme
 <*> strArgument (metavar "REPO_OWNER" <> help "Owner of the GitHub repository")
 <*> strArgument (metavar "REPO_NAME" <> help "Name of the GitHub repository")
 <*> argument auto (metavar "ISSUE_NUMBER" <> help "ID of the GitHub pull request")

pAuthScheme :: Parser AuthScheme
pAuthScheme = pOAuthScheme <|> pNoAuthScheme
  where
    pNoAuthScheme =
      flag' NoAuthScheme
        (  long "no-auth"
        <> help "Don't authenticate with GitHub (beware of rate-limiting on reads)."
        )
    pOAuthScheme =
      OAuthScheme <$> 
        option auto
          (  long "oauth"
          <> help "Authenticate using an OAuth token."
          )
