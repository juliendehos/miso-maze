{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (newTVarIO)
import Data.Maybe (fromMaybe)
import Network.WebSockets qualified as WS
import System.Environment (lookupEnv)

import Server.WsModel
import Server.WsApp

main :: IO ()
main = do
  port <- read . fromMaybe "9000" <$> lookupEnv "PORT"
  putStrLn $ "Running on port " <> show port
  wsVar <- newTVarIO =<< mkWsModel "mazes"
  void $ forkIO $ runBot wsVar 
  WS.runServer "0.0.0.0" port $ wsApp wsVar

