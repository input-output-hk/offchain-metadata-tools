module Cardano.Metadata.Store.Postgres.Config where

import qualified Data.ByteString.Char8 as BC
import Database.Persist.Postgresql ( ConnectionString )
import qualified Network.Wai.Handler.Warp as Warp
import Options.Applicative

data Opts = Opts
    { optDbName              :: String
    , optDbUser              :: String
    , optDbPass              :: String
    , optDbSslMode           :: String
    , optDbHost              :: FilePath
    , optDbMetadataTableName :: String
    , optDbConnections       :: Int
    , optServerPort          :: Warp.Port
    }
    deriving (Eq, Show)

parseOpts :: Parser Opts
parseOpts = Opts
  <$> strOption (long "db" <> metavar "DB_NAME" <> help "Name of the database to store and read metadata from")
  <*> strOption (long "db-user" <> metavar "DB_USER" <> help "User to connect to metadata database with")
  <*> strOption (long "db-pass" <> metavar "DB_PASS" <> help "User password to connect to metadata database with")
  <*> strOption (long "db-ssl-mode" <> metavar "DB_SSL_MODE" <> help "SSL mode for the db connection; ex: disable, allow, prefer, require, verify-ca, verify-full")
  <*> strOption (long "db-host" <> metavar "DB_HOST" <> showDefault <> value "/run/postgresql" <> help "Host for the metadata database connection")
  <*> strOption (long "db-table" <> metavar "DB_TABLE" <> showDefault <> value "metadata" <> help "Table in the database to store metadata")
  <*> option auto (long "db-conns" <> metavar "INT" <> showDefault <> value 1 <> help "Number of connections to open to the database")
  <*> option auto (short 'p' <> long "port" <> metavar "PORT" <> showDefault <> value 8080 <> help "Port to run the metadata web server on")

pgConnectionString :: Opts -> ConnectionString
pgConnectionString (Opts { optDbName = dbName, optDbUser = dbUser, optDbPass = dbPass, optDbSslMode = dbSslMode, optDbHost = dbHost }) =
  BC.pack $ "host=" <> dbHost <> " dbname=" <> dbName <> " user=" <> dbUser <> " password=" <> dbPass <> " sslmode=" <> dbSslMode
