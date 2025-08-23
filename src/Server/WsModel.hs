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
import Miso.Lens.TH
import Miso.String (strip)
import Network.WebSockets qualified as WS
import System.Directory (listDirectory)
import System.Random (getStdGen)

import Game.Game
import Network.Api

data WsModel = WsModel
  { _wsGame :: Game
  , _wsClients :: M.Map MisoString WS.Connection
  , _wsCurrentBoard :: MisoString
  , _wsBoardFiles :: [MisoString]
  }

makeLenses ''WsModel

mkWsModel :: MisoString -> IO WsModel
mkWsModel mazeDir = do
  let dir = T.unpack $ mkStaticUri mazeDir 
      mazeDir' = mazeDir <> "/"
  mazeFiles <- map ((mazeDir'<>) . T.pack) . sort <$> listDirectory dir
  T.putStrLn $ "maze files: " <> T.unwords mazeFiles
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
      str <- T.readFile $ T.unpack $ mkStaticUri f
      case loadBoard str _wsGame of
        Left err -> do
          T.putStrLn $ "failed to read " <> f <> ": " <> err
          nextBoard $ ws & wsBoardFiles .~ fs   -- remove board file and try next 
        Right game -> do
          let boardFile = ms f
          T.putStrLn $ "new board: " <> boardFile
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

