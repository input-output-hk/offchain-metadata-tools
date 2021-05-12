module Cardano.Metadata.Store.File.Config where

import qualified Network.Wai.Handler.Warp as Warp
import Options.Applicative

data Opts = Opts
    { optMetadataLocation :: FilePath
    , optServerPort       :: Warp.Port
    }
    deriving (Eq, Show)

parseOpts :: Parser Opts
parseOpts = Opts
  <$> strOption (long "folder" <> metavar "FOLDER" <> help "Folder containing the metadata entries")
  <*> option auto (short 'p' <> long "port" <> metavar "PORT" <> showDefault <> value 8080 <> help "Port to run the metadata web server on")
