{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Colog
import Prelude hiding (log)
import qualified Options.Applicative as Opt
import Data.Aeson (FromJSON)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (runExceptT, ExceptT, MonadError)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import System.Exit (exitSuccess, exitFailure)
import qualified GitHub
import qualified Data.Vector as Vector
import qualified Data.Text as T

import Cardano.Metadata.Validation.GitHub (PullRequestValidationError, gitHubValidationRules, prettyPrintPRValidationError, fromGHFile, fromGHPullRequest)
import Config (Config(Config), AuthScheme(NoAuthScheme, OAuthScheme), opts, mkConfig)

data Env m = Env { envLogAction  :: !(LogAction m Message) }

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newLogAction env = env { envLogAction = newLogAction }
  {-# INLINE setLogAction #-}

newtype App a = App
    { unApp :: ReaderT (Env App) (ExceptT PullRequestValidationError IO) a
    } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (Env App), MonadError PullRequestValidationError)

runApp :: Env App -> App a -> IO (Either PullRequestValidationError a)
runApp env = runExceptT . flip runReaderT env . unApp

main :: IO ()
main = do
  (Config authScheme expectedBaseBranch repoOwner repoName prNumber logSeverity)
    <- mkConfig <$> Opt.execParser opts

  let
    action :: MonadIO m => LogAction m Message
    action = filterBySeverity logSeverity msgSeverity (cmap fmtMessage logTextStdout)

  result <- runApp (Env action) $ do
    pr      <- run' authScheme $ GitHub.pullRequestR repoOwner repoName prNumber
    prFiles <- run' authScheme $ GitHub.pullRequestFilesR repoOwner repoName prNumber (GitHub.FetchAtLeast 1)

    gitHubValidationRules expectedBaseBranch (fromGHPullRequest pr) (Vector.toList $ fromGHFile <$> prFiles)

  case result of
    Left err -> error $ T.unpack $ prettyPrintPRValidationError err
    Right () -> exitSuccess

run' :: forall env m req. (WithLog env Message m, MonadIO m, FromJSON req) => AuthScheme -> GitHub.Request ('GitHub.RO :: GitHub.RW) req -> m req
run' authScheme req = do
  let
    go = case authScheme of
      NoAuthScheme      -> GitHub.github'
      OAuthScheme token -> GitHub.github (GitHub.OAuth token)

  res <- liftIO $ go req
  case res of
    Left err -> do
      log E $ "GitHub error: '" <> T.pack (show err) <> "'."
      liftIO exitFailure
    Right x  -> pure x
