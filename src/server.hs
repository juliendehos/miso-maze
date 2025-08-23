{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newTVarIO)
import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (GzipFiles (..), def, gzip, gzipFiles)
-- import Network.Wai.Middleware.RequestLogger (logStdout)
import System.Environment (lookupEnv)

import Server.WsModel
import Server.ServerApp
import Server.WsApp

main :: IO ()
main = do
  port <- read . fromMaybe "3000" <$> lookupEnv "PORT"
  putStrLn $ "Running on port " <> show port
  wsVar <- newTVarIO =<< mkWsModel "mazes"
  void $ forkIO $ runBot wsVar 
  run port
    -- $ logStdout 
    $ wsApp wsVar
    $ compress serverApp
  where
    compress = gzip def{gzipFiles = GzipCompress}

