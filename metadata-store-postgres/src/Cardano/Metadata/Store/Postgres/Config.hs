module Cardano.Metadata.Store.Postgres.Config where

import qualified Data.ByteString.Char8 as BC
import Database.Persist.Postgresql ( ConnectionString )
import qualified Network.Wai.Handler.Warp as Warp
import Control.Applicative (optional)
import Options.Applicative

data Opts = Opts
    { optDbName              :: String
    , optDbUser              :: String
    , optDbPass              :: Maybe String
    , optDbHost              :: String
    , optDbPort              :: Maybe Warp.Port
    , optDbMetadataTableName :: String
    , optDbConnections       :: Int
    , optServerPort          :: Warp.Port
    }
    deriving (Eq, Show)

parseOpts :: Parser Opts
parseOpts = Opts
  <$> strOption (long "db" <> metavar "DB_NAME" <> help "Name of the database to store and read metadata from")
  <*> strOption (long "db-user" <> metavar "DB_USER" <> help "User to connect to metadata database with")
  <*> optional (strOption (long "db-pass" <> metavar "DB_PASS" <> help "Password to connect to metadata database with"))
  <*> strOption (long "db-host" <> metavar "DB_HOST" <> showDefault <> value "/run/postgresql" <> help "Host for the metadata database connection")
  <*> optional (option auto (long "db-port" <> metavar "DB_PORT" <> showDefault <> value 5432 <> help "Port for the metadata database connection"))
  <*> strOption (long "db-table" <> metavar "DB_TABLE" <> showDefault <> value "metadata" <> help "Table in the database to store metadata")
  <*> option auto (long "db-conns" <> metavar "INT" <> showDefault <> value 1 <> help "Number of connections to open to the database")
  <*> option auto (short 'p' <> long "port" <> metavar "PORT" <> showDefault <> value 8080 <> help "Port to run the metadata web server on")

pgConnectionString :: Opts -> ConnectionString
pgConnectionString (Opts { optDbName = dbName, optDbUser = dbUser, optDbPass = dbPass, optDbHost = dbHost, optDbPort = dbPort }) =
  BC.pack $ "host=" <> dbHost <> " dbname=" <> dbName <> " user=" <> dbUser <> (maybe "" (" password=" <>) dbPass) <> (maybe "" ((" port=" <>) . show) dbPort)
