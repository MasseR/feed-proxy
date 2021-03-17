{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified MyLib (defaultMain)

import Data.Environment (Environment(..))

import Options.Generic

import Network.HTTP.Client.TLS (newTlsManager)

data Options
  = Options { port :: Int
            , cache :: FilePath
            }

deriving stock instance Generic Options
deriving anyclass instance ParseRecord Options

main :: IO ()
main = do
  Options{..} <- getRecord "feed-proxy"
  env <- Environment <$> newTlsManager
  MyLib.defaultMain port env
