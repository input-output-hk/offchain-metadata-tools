{-# OPTIONS_HADDOCK show-extensions #-}

{- |
  Module – Module name
  Description – Short description
  Maintainer – samuel.evans-powell@iohk.io
  Stability – experimental
-}

module Main where

import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Logger
    ( runStdoutLoggingT )
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Database.Persist.Postgresql as Postgresql
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative as Opt

import Cardano.Metadata.Server
    ( webApp )
import qualified Cardano.Metadata.Store.File as Store
import Cardano.Metadata.Store.File.Config
    ( Opts (..) )
import Config
    ( opts )

main :: IO ()
main = do
  options@(Opts { optMetadataLocation = folder
                , optServerPort = port
                }) <- Opt.execParser opts

  putStrLn $ "Using file store at: " <> folder
  intf <- Store.fileStore folder
  putStrLn $ "Metadata server is starting on port " <> show port <> "."
  liftIO $ Warp.run port (webApp intf)
