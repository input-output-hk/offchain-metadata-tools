module Config where

import Options.Applicative
import qualified Data.ByteString     as BS
import Data.Text (Text)
import qualified GitHub as GitHub
import qualified GitHub.Data.Name as GitHub
import qualified Colog

data AuthScheme = NoAuthScheme
                | OAuthScheme BS.ByteString

data Opts
  = Opts { optAuth        :: AuthScheme
         , optRepoOwner   :: Text
         , optRepoName    :: Text
         , optIssueNumber :: Int
         , optLogSeverity :: Colog.Severity
         }

data Config
  = Config { cfgAuth        :: AuthScheme
           , cfgRepoOwner   :: GitHub.Name GitHub.Owner
           , cfgRepoName    :: GitHub.Name GitHub.Repo
           , cfgIssueNumber :: GitHub.IssueNumber
           , cfgLogSeverity :: Colog.Severity
           }

mkConfig :: Opts -> Config
mkConfig (Opts auth owner name issue severity) =
  Config auth (GitHub.N owner) (GitHub.N name) (GitHub.IssueNumber issue) severity

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
 <*> pLogSeverity

pAuthScheme :: Parser AuthScheme
pAuthScheme = pOAuthScheme <|> pNoAuthScheme
  where
    pNoAuthScheme =
      flag' NoAuthScheme
        (  long "no-auth"
        <> help "Don't authenticate with GitHub (beware of rate-limiting on reads)"
        )
    pOAuthScheme =
      OAuthScheme <$>
        strOption
          (  long "oauth"
          <> help "Authenticate using an OAuth token"
          )

pLogSeverity :: Parser Colog.Severity
pLogSeverity = pDebug <|> pInfo <|> pWarning <|> pError <|> pure Colog.Info
  where
    pDebug =
      flag' Colog.Debug
        (  long "debug"
        <> help "Print debug, info, warning, and error messages"
        )
    pInfo =
      flag' Colog.Info
        (  long "info"
        <> help "Print info, warning, and error messages"
        )
    pWarning =
      flag' Colog.Warning
        (  long "warning"
        <> help "Print warning, and error messages"
        )
    pError =
      flag' Colog.Error
        (  long "error"
        <> help "Print error messages only"
        )
