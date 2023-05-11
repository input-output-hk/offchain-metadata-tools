{-# LANGUAGE OverloadedStrings #-}

module Cardano.Metadata.Sync.Config where

import Control.Exception ( bracket )
import qualified Data.ByteString.Char8 as BC
import Data.Pool ( Pool, createPool, destroyAllResources )
import qualified Data.Pool as Pool
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock ( NominalDiffTime )
import Database.PostgreSQL.Simple ( Connection )
import qualified Database.PostgreSQL.Simple as Sql
import qualified Network.Wai.Handler.Warp as Warp
import Options.Applicative
    ( Parser
    , ParserInfo
    , auto
    , fullDesc
    , header
    , help
    , info
    , long
    , metavar
    , option
    , progDesc
    , showDefault
    , strOption
    , value
    )
import qualified Options.Applicative as Opt

data Opts = Opts
    { optDbName              :: Text
    , optDbUser              :: Text
    , optDbPass              :: Text
    , optDbHost              :: String
    , optDbPort              :: Warp.Port
    , optDbMetadataTableName :: Text
    , optDbConnections       :: Int
    , optGitURL              :: Text
    , optGitSubFolder        :: Text
    }
    deriving (Eq, Show)

parseOpts :: Parser Opts
parseOpts = Opts
  <$> strOption (long "db" <> metavar "DB_NAME" <> help "Name of the database to store and read metadata from")
  <*> strOption (long "db-user" <> metavar "DB_USER" <> help "User to connect to metadata database with")
  <*> strOption (long "db-pass" <> metavar "DB_PASS" <> help "Password to connect to metadata database with")
  <*> strOption (long "db-host" <> metavar "DB_HOST" <> showDefault <> value "/run/postgresql" <> help "Host for the metadata database connection")
  <*> strOption (long "db-port" <> metavar "DB_PORT" <> showDefault <> value 5432 <> help "Port for the metadata database connection")
  <*> strOption (long "db-table" <> metavar "DB_TABLE" <> showDefault <> value "metadata" <> help "Table in the database to store metadata")
  <*> option auto (long "db-conns" <> metavar "INT" <> showDefault <> value 1 <> help "Number of connections to open to the database")
  <*> strOption (long "git-url" <> metavar "GIT_URL" <> help "URL of the metadata registry git repository")
  <*> strOption (long "git-metadata-folder" <> metavar "GIT_METADATA_FOLDER" <> help "Sub-folder of the git repository containing the metadata")

opts :: ParserInfo Opts
opts =
  info
    parseOpts
    ( fullDesc
    <> progDesc "Sync up a metadata database with a GitHub repository"
    <> header "metadata-sync - a tool to keep the metadata storage layer and the GitHub repository in sync"
    )

pgConnectionString :: Opts -> BC.ByteString
pgConnectionString (Opts { optDbName = dbName, optDbUser = dbUser, optDbPass = dbPass, optDbHost = dbHost, optDbPort = dbPort }) =
  TE.encodeUtf8 $ "host=" <> T.pack dbHost <> " dbname=" <> dbName <> " user=" <> dbUser " password=" <> dbPass " port=" <> dbPort

mkConnectionPool
  :: BC.ByteString
  -- ^ Libpq connection string
  -> Int
  -- ^ Maximum number of postgresql connections to allow
  -> IO (Pool Connection)
mkConnectionPool connectionStr numConns =
  createPool
    (Sql.connectPostgreSQL connectionStr)
    Sql.close
    1                       -- Number of sub-pools
    (10 :: NominalDiffTime) -- Amount of time for which an unused connection is kept open
    numConns

withConnectionPool
  :: BC.ByteString
  -- ^ Libpq connection string
  -> Int
  -- ^ Maximum number of postgresql connections to allow
  -> (Pool Connection -> IO r)
  -> IO r
withConnectionPool connectionInfo numConns f = bracket
  (mkConnectionPool connectionInfo numConns)
  destroyAllResources
  f

withConnectionFromPool :: Pool Connection -> (Connection -> IO b) -> IO b
withConnectionFromPool pool action = Pool.withResource pool $ action
