{-# OPTIONS_HADDOCK show-extensions #-}

{- |
  Module – Module name
  Description – Short description
  Maintainer – samuel.evans-powell@iohk.io
  Stability – experimental
-}

module Main where

import Data.Text (Text)

main :: IO ()
main = 
  port <- maybe 8080 read <$> lookupEnv "METADATA_SERVER_PORT"
  putStrLn $ "Server is starting on port " <> show port <> "."
  run port (app Postgres.readFns)


