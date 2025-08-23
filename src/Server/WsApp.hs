{-# LANGUAGE OverloadedStrings #-}

module Server.WsApp (wsApp, runBot) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever, forM_, when)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy qualified as B
import Data.Map qualified as M
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Miso
import Miso.Lens
import Network.WebSockets qualified as WS
import System.Random.Stateful

import Game.Common
import Game.Game
import Network.Protocol
import Server.WsModel

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

recvMsg :: WS.Connection -> IO (Maybe ClientMsg)
recvMsg conn = decode . WS.fromLazyByteString <$> WS.receiveData conn

sendMsg :: WS.Connection -> ServerMsg -> IO ()
sendMsg conn = WS.sendTextData conn . T.decodeUtf8 . B.toStrict . encode

broadcastPlayers :: TVar WsModel -> IO ()
broadcastPlayers wsVar = do
  (game, conns) <- atomically $ do
    ws <- readTVar wsVar
    pure (ws^.wsGame, ws^.wsClients&M.elems)
  forM_ conns $ flip sendMsg (UpdateInfos (_gameRemMeetings game) (getPlayers game))

broadcastBoard :: TVar WsModel -> IO ()
broadcastBoard wsVar = do
  (currentBoard, conns) <- atomically $ do
    ws <- readTVar wsVar
    pure (ws^.wsCurrentBoard, ws^.wsClients&M.elems)
  forM_ conns $ flip sendMsg (UpdateBoard $ ms currentBoard)

playClientMove :: TVar WsModel -> MisoString -> Move -> IO ()
playClientMove wsVar name move = do

  (gameChanged, gameOver) <- atomically $ do
    ws <- readTVar wsVar
    case playMove name move (ws^.wsGame) of
      Nothing -> pure (False, False)
      Just game -> do
        writeTVar wsVar (ws & wsGame .~ game)
        pure (True, not $ isRunning game)

  when gameChanged $ do
    when gameOver $ do
      -- TODO race condition?
      model <- readTVarIO wsVar
      model' <- nextBoard model
      atomically $ writeTVar wsVar model'
      broadcastBoard wsVar

    broadcastPlayers wsVar

-------------------------------------------------------------------------------
-- handlers
-------------------------------------------------------------------------------

handleConnect :: TVar WsModel -> WS.Connection -> IO ()
handleConnect wsVar conn = do
  msg <- recvMsg conn
  case msg of
    Just (Join name) -> do
      res <- atomically $ tryAddClient name conn wsVar
      case res of
        Left err -> do
          T.putStrLn $ "rejected: " <> err
          sendMsg conn (Rejected err)
        Right name' -> do
          T.putStrLn $ name' <> " has joined the maze"
          finally (handleRun wsVar name' conn) (handleClose wsVar name')
    _ -> do
      T.putStrLn "rejected: expecting Join"
      sendMsg conn (Rejected "expecting Join")

handleClose :: TVar WsModel -> MisoString -> IO ()
handleClose wsVar name = do
  atomically $ modifyTVar' wsVar $ \ws -> 
    ws & wsClients %~ M.delete name
       & wsGame %~ deletePlayer name
  broadcastPlayers wsVar
  T.putStrLn $ name <> " left the maze"

handleRun :: TVar WsModel -> MisoString -> WS.Connection -> IO ()
handleRun wsVar name conn = do

  -- get and send the current board
  boardUrl <- atomically (_wsCurrentBoard <$> readTVar wsVar)
  sendMsg conn (UpdateBoard $ ms boardUrl)

  -- get and broadcast the player list
  broadcastPlayers wsVar

  -- handle requests from the client
  forever $ do
    msg <- recvMsg conn
    case msg of
      Just (Play move) -> playClientMove wsVar name move
      _ -> T.putStrLn "ignored: expecting Play"

-------------------------------------------------------------------------------
-- app
-------------------------------------------------------------------------------

wsApp ::TVar WsModel -> WS.PendingConnection -> IO ()
wsApp wsVar pc = do
  conn <- WS.acceptRequest pc
  WS.withPingThread conn 30 (pure ()) (handleConnect wsVar conn)

-------------------------------------------------------------------------------
-- bot
-------------------------------------------------------------------------------

runBot :: TVar WsModel -> IO ()
runBot wsVar = do
  res <- atomically $ tryAddBot "random bot" wsVar
  case res of
    Left err -> T.putStrLn $ "failed to run bot: " <> err
    Right name -> forever $ do
      threadDelay 1_000_000
      move <- uniformM globalStdGen
      playClientMove wsVar name move

