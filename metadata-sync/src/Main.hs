module Main where

import Control.Monad.IO.Class
    ( liftIO )
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative as Opt
import Control.Exception (bracket)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close)
import Data.Pool (Pool, createPool, destroyAllResources)
import Data.Time.Clock (NominalDiffTime)

import qualified Cardano.Metadata.Sync as Sync
import Cardano.Metadata.Sync.Config
    ( Opts (..), pgConnectionString, parseOpts, withConnectionPool, withConnectionFromPool, opts)

main :: IO ()
main = do
  options@(Opts { optDbConnections = numDbConns
                , optDbMetadataTableName = tableName
                , optGitURL = gitURL
                , optGitSubFolder = gitSubFolder
                }) <- Opt.execParser opts

  let pgConnString = pgConnectionString options
  putStrLn $ "Connecting to database using connection string: " <> BC.unpack pgConnString
  withConnectionPool pgConnString numDbConns $ \pool -> do
    withConnectionFromPool pool $ \conn -> do
      putStrLn $ "Reading registry state from '" <> T.unpack gitURL <> "'."
      state <- Sync.view gitURL gitSubFolder
  
      putStrLn $ "Syncing to table '" <> T.unpack tableName <> "'."
      Sync.write conn tableName state
