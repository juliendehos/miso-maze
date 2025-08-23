{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server.WsModel where

import Control.Concurrent.STM
import Data.List (sort)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Miso
import Miso.Lens
-- import Miso.Lens.TH
import Miso.String (strip)
import Network.WebSockets qualified as WS
import System.Directory (listDirectory)
import System.Random (getStdGen)

import Game.Game

import Paths_miso_maze

data WsModel = WsModel
  { _wsGame :: Game
  , _wsClients :: M.Map MisoString WS.Connection
  , _wsCurrentBoard :: FilePath
  , _wsBoardFiles :: [FilePath]
  }

-- makeLenses ''WsModel
wsGame = lens _wsGame (\ record field -> record {_wsGame = field})
wsGame :: Lens WsModel Game 
wsClients = lens _wsClients (\ record field -> record {_wsClients = field})
wsClients :: Lens WsModel (M.Map MisoString WS.Connection)
wsCurrentBoard = lens _wsCurrentBoard (\ record field -> record {_wsCurrentBoard = field})
wsCurrentBoard :: Lens WsModel FilePath
wsBoardFiles = lens _wsBoardFiles (\ record field -> record {_wsBoardFiles = field})
wsBoardFiles :: Lens WsModel [FilePath]

mkWsModel :: FilePath -> IO WsModel
mkWsModel mazeDir = do
  dataDir <- getDataDir
  let dir = dataDir <> "/" <> mazeDir 
      mazeDir' = mazeDir <> "/"
  mazeFiles <- map (mazeDir'<>) . sort <$> listDirectory dir
  putStrLn $ "maze files: " <> unwords mazeFiles
  gen <- getStdGen
  let model0 = WsModel (mkGame gen) M.empty " " mazeFiles
  nextBoard model0

nextBoard :: WsModel -> IO WsModel
nextBoard ws@WsModel{..} = do 
  case _wsBoardFiles of
    [] -> do
      T.putStrLn "no more board"
      pure ws
    (f:fs) -> do
      dataDir <- getDataDir
      str <- T.readFile $ dataDir <> "/" <> f
      case loadBoard str _wsGame of
        Left err -> do
          putStrLn $ "failed to read " <> f <> ": " <> T.unpack err
          nextBoard $ ws & wsBoardFiles .~ fs   -- remove board file and try next 
        Right game -> do
          let boardFile = f
          putStrLn $ "new board: " <> boardFile
          pure $ ws & wsBoardFiles .~ fs ++ [f]
                    & wsCurrentBoard .~ boardFile
                    & wsGame .~ game

tryAddClient
  :: MisoString
  -> WS.Connection
  -> TVar WsModel
  -> STM (Either MisoString MisoString)
tryAddClient name conn wsVar = do
  let name' = strip name
  ws <- readTVar wsVar
  case tryAddPlayer name' (ws^.wsGame) of
    Left err -> pure $ Left err
    Right game -> do
      let ws' = ws & wsClients %~ M.insert name' conn 
                   & wsGame .~ game
      writeTVar wsVar ws'
      pure $ Right name'

tryAddBot
  :: MisoString
  -> TVar WsModel
  -> STM (Either MisoString MisoString)
tryAddBot name wsVar = do
  let name' = strip name
  ws <- readTVar wsVar
  case tryAddPlayer name' (ws^.wsGame) of
    Left err -> pure $ Left err
    Right game -> do
      let ws' = ws & wsGame .~ game
      writeTVar wsVar ws'
      pure $ Right name'


