{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified MyLib
       (defaultMain)

import Data.Environment
       (Environment(..))
import System.Directory.HasCache
       (Cache(..))

import Options.Generic

import Network.HTTP.Client.TLS
       (newTlsManager)

import Control.Monad.Logger
       (Logger(..))
import Katip
       ( ColorStrategy(..)
       , Severity(..)
       , Verbosity(..)
       , closeScribes
       , defaultScribeSettings
       , initLogEnv
       , mkHandleScribe
       , permitItem
       , registerScribe
       )
import System.IO
       (stdout)

import Control.Exception
       (bracket)

data Options
  = Options { port :: Int
            , cache :: FilePath
            }

deriving stock instance Generic Options
deriving anyclass instance ParseRecord Options

withStdoutLogger :: (Logger -> IO a) -> IO a
withStdoutLogger f = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  let makeLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "feed-proxy" "production"
  -- closeScribes will stop accepting new logs, flush existing ones and clean up resources
  bracket makeLogEnv closeScribes $ \le -> do
    let initialContext = mempty -- this context will be attached to every log in your app and merged w/ subsequent contexts
    let initialNamespace = "main"
    f (Logger initialNamespace initialContext le)

main :: IO ()
main = withStdoutLogger $ \logger -> do
  Options{..} <- getRecord "feed-proxy"
  env <- Environment <$> newTlsManager <*> pure (Cache cache) <*> pure logger
  MyLib.defaultMain port env
