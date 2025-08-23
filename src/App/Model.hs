{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Model where

import Miso
import Miso.Lens
import Miso.Lens.TH
import Miso.WebSocket

import Game.Common

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

makeLenses ''Model

mkModel :: MisoString -> Model
mkModel wsUrl = Model 
  { _modelError = " "
  , _modelWsUrl = wsUrl
  , _modelWs = emptyWebSocket
  , _modelName = " "
  , _modelPlayers = []
  , _modelBoard = emptyBoard
  , _modelBoardName = " "
  , _modelRemMeetings = 0
  }

