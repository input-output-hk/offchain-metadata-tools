{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Metadata.Store.Postgres where

import           Prelude hiding (init, read)
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Control.Monad.Reader
import          Data.Text (Text)
import           Database.Persist.Sql
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Control.Monad.Logger (logInfoN, runNoLoggingT, runStderrLoggingT,
                     runStdoutLoggingT)

import Cardano.Metadata.Store.Types 
import Database.Persist.Sql
import Data.Pool
import Data.Word
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Aeson (ToJSONKey, ToJSON)
import qualified Data.Aeson as Aeson

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Record
    key String
    value String
  deriving Show
|]

data KeyValue = KeyValue { _kvConnPool :: Pool SqlBackend
                         }

x :: ReaderT SqlBackend IO [Text]
x = showMigration migrateAll

init :: (ToJSONKey k, ToJSON v) => ConnectionPool -> Map k v -> IO KeyValue
init pool state = do
  withResource pool $ \backend -> do
    mig <- runReaderT x backend
    traverse_ (putStrLn . show) mig
    pure $ KeyValue pool

m :: IO KeyValue
m =
  runNoLoggingT $
    withPostgresqlPool "host=/run/postgresql dbname=cexplorer user=cardano-node" 1 $ \pool -> do
      liftIO $ init pool (M.singleton 1 1 :: Map Word8 Word8)
