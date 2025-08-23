{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Model where

import Miso
import Miso.Lens
-- import Miso.Lens.TH
import Miso.WebSocket

import Game.Common

defaultWsUrl :: MisoString
defaultWsUrl = "ws://127.0.0.1:9000"

data Model = Model
  { _modelError :: MisoString
  , _modelWsUrl :: MisoString
  , _modelWs :: WebSocket
  , _modelName :: MisoString
  , _modelPlayers :: [Player]
  , _modelBoard :: Board
  , _modelBoardName :: MisoString
  , _modelRemMeetings :: Int
  } deriving (Eq)

-- makeLenses ''Model
modelError = lens _modelError (\ record field -> record {_modelError = field})
modelError :: Lens Model MisoString
modelWsUrl = lens _modelWsUrl (\ record field -> record {_modelWsUrl = field})
modelWsUrl :: Lens Model MisoString
modelWs = lens _modelWs (\ record field -> record {_modelWs = field})
modelWs :: Lens Model WebSocket
modelName = lens _modelName (\ record field -> record {_modelName = field})
modelName :: Lens Model MisoString
modelPlayers = lens _modelPlayers (\ record field -> record {_modelPlayers = field})
modelPlayers :: Lens Model [Player]
modelBoard = lens _modelBoard (\ record field -> record {_modelBoard = field})
modelBoard :: Lens Model Board
modelBoardName = lens _modelBoardName (\ record field -> record {_modelBoardName = field})
modelBoardName :: Lens Model MisoString
modelRemMeetings = lens _modelRemMeetings (\ record field -> record {_modelRemMeetings = field})
modelRemMeetings :: Lens Model Int


mkModel :: Model
mkModel = Model 
  { _modelError = ""
  , _modelWsUrl = defaultWsUrl
  , _modelWs = emptyWebSocket
  , _modelName = " "
  , _modelPlayers = []
  , _modelBoard = emptyBoard
  , _modelBoardName = ""
  , _modelRemMeetings = 0
  }

