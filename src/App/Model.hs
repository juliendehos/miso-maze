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

-------------------------------------------------------------------------------
-- lenses
-------------------------------------------------------------------------------

-- makeLenses ''Model

modelError :: Lens Model MisoString
modelError = lens _modelError (\ record field -> record {_modelError = field})

modelWsUrl :: Lens Model MisoString
modelWsUrl = lens _modelWsUrl (\ record field -> record {_modelWsUrl = field})

modelWs :: Lens Model WebSocket
modelWs = lens _modelWs (\ record field -> record {_modelWs = field})

modelName :: Lens Model MisoString
modelName = lens _modelName (\ record field -> record {_modelName = field})

modelPlayers :: Lens Model [Player]
modelPlayers = lens _modelPlayers (\ record field -> record {_modelPlayers = field})

modelBoard :: Lens Model Board
modelBoard = lens _modelBoard (\ record field -> record {_modelBoard = field})

modelBoardName :: Lens Model MisoString
modelBoardName = lens _modelBoardName (\ record field -> record {_modelBoardName = field})

modelRemMeetings :: Lens Model Int
modelRemMeetings = lens _modelRemMeetings (\ record field -> record {_modelRemMeetings = field})

