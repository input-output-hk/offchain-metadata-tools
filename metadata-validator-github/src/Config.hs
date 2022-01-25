module Config where

import qualified Colog
import qualified Data.ByteString as BS
import Data.Text ( Text )
import qualified Data.Text as T
import qualified GitHub
import qualified GitHub.Data.Name as GitHub
import Options.Applicative

import Cardano.Metadata.Validation.GitHub
    ( ExpectedBaseBranch (ExpectedBaseBranch) )

data AuthScheme = NoAuthScheme
                | OAuthScheme BS.ByteString

data Opts
  = Opts { optAuth             :: AuthScheme
         , optExpectBaseBranch :: ExpectedBaseBranch
         , optRepoOwner        :: Text
         , optRepoName         :: Text
         , optIssueNumber      :: Int
         , optLogSeverity      :: Colog.Severity
         }

data Config
  = Config { cfgAuth             :: AuthScheme
           , cfgExpectBaseBranch :: ExpectedBaseBranch
           , cfgRepoOwner        :: GitHub.Name GitHub.Owner
           , cfgRepoName         :: GitHub.Name GitHub.Repo
           , cfgIssueNumber      :: GitHub.IssueNumber
           , cfgLogSeverity      :: Colog.Severity
           }

mkConfig :: Opts -> Config
mkConfig (Opts auth expectBranch owner name issue severity) =
  Config auth expectBranch (GitHub.N owner) (GitHub.N name) (GitHub.IssueNumber issue) severity

opts :: ParserInfo Opts
opts =
  info
    parseOpts
    ( fullDesc
    <> progDesc "Validate a pull request against a GitHub repository."
    <> header "metadata-validator-github - a tool to validate that metadata entry pull requests are in correct form."
    )

parseOpts :: Parser Opts
parseOpts = Opts
 <$> pAuthScheme
 <*> (ExpectedBaseBranch . T.pack <$> strOption (long "expect-branch" <> help "All PRs must be against this branch." <> showDefault <> value "main"))
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
