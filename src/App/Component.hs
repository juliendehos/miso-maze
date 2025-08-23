{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Component where

import Control.Monad (forM_, when)
import Data.IntSet qualified as IS
import Miso
import Miso.Canvas (Canvas, canvas)
import Miso.Canvas qualified as Canvas 
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P
import Miso.Lens
import Miso.CSS qualified as CSS
import Miso.String (strip)
import Miso.WebSocket

import App.Model 
import Game.Common
import Network.Protocol

-------------------------------------------------------------------------------
-- params
-------------------------------------------------------------------------------

cellSize :: Int
cellSize = 20

backgroundColor, wallColor :: CSS.Color
backgroundColor   = CSS.hex 0x88DD88
wallColor         = CSS.hex 0x2277DD

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

data Action
  = ActionNone
  | ActionDebug MisoString
  | ActionKey IS.IntSet
  | ActionServerURL MisoString
  | ActionName MisoString
  | ActionConnect
  | ActionSetBoard MisoString MisoString    -- url, data
  | ActionWsOpen WebSocket
  | ActionWsClosed Closed
  | ActionWsError MisoString
  | ActionWsRecv ServerMsg
  | ActionWsSend ClientMsg

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

debug :: MisoString -> Transition Model Action
debug str = io_ (consoleLog str)

-------------------------------------------------------------------------------
-- Update
-------------------------------------------------------------------------------

updateModel :: Action -> Transition Model Action

updateModel ActionNone =
  pure ()

updateModel (ActionDebug str) =
  debug str

updateModel (ActionKey keys)
  | IS.member 37 keys = issue $ ActionWsSend (Play MoveLeft)
  | IS.member 38 keys = issue $ ActionWsSend (Play MoveUp)
  | IS.member 39 keys = issue $ ActionWsSend (Play MoveRight)
  | IS.member 40 keys = issue $ ActionWsSend (Play MoveDown)
  | otherwise = pure ()

updateModel (ActionServerURL str) =
  modelWsUrl .= str

updateModel (ActionName str) =
  modelName .= str

updateModel ActionConnect = do
  name <- strip <$> use modelName
  if name == ""
    then modelError .= "Please, enter a valid name."
    else do
      modelError .= ""
      wsUrl <- use modelWsUrl
      debug wsUrl
      connectJSON wsUrl ActionWsOpen ActionWsClosed ActionWsRecv ActionWsError

updateModel (ActionSetBoard url str) =
  case parseBoard str of
    Left err -> modelError .= "Failed to parse board: " <> err
    Right board -> do
      modelBoard .= board
      modelBoardName .= url

updateModel (ActionWsOpen socket) = do
  modelWs .= socket
  name <- strip <$> use modelName
  issue $ ActionWsSend (Join name)

updateModel (ActionWsClosed c) = do
  debug $ "updateModel ActionWsClosed: " <> ms (show c)
  modelPlayers .= []

updateModel (ActionWsError err) = do
  modelError .= "WebSocket error"
  debug $ "updateModel ActionWsError: " <> err

updateModel (ActionWsRecv json) =
  case json of
    Rejected err -> modelError .= "Error: " <> err
    UpdateInfos n ps -> do
      modelRemMeetings .= n
      modelPlayers .= ps
    UpdateBoard url -> do
      debug $ "updateModel ActionWsRecv: " <> url 
      getText url [] (ActionSetBoard url) ActionDebug

updateModel (ActionWsSend msg) = do
  socket <- use modelWs
  sendJSON socket msg

-------------------------------------------------------------------------------
-- View
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel m@Model{..} =
  div_ 
    []
    [ if _modelBoard == emptyBoard then viewConnect else viewRun _modelBoard
    , p_ [] [ text _modelError ]
    ]

  where
    viewConnect =
      div_ []
        [ p_ [] 
            [ "Server URL: "
            , input_ [ type_ "text", value_ defaultWsUrl, onChange ActionServerURL ]
            ]
        , p_ []
            [ "Name: "
            , input_ [ type_ "text", onChange ActionName ]
            ]
        , button_ [ onClick ActionConnect ] [ "connect" ]
        ]

    viewRun Board{..} =
      let 
        (ni, nj) = _boardNiNj
        w = fromIntegral $ cellSize * nj
        h = fromIntegral $ cellSize * ni
      in div_ []
          [ p_ [] [ "Meet other players, using keyboard arrows." ]
          , canvas 
              [ width_ (ms $ show w)
              , height_ (ms $ show h)
              , CSS.style_  [CSS.border "1px solid black"]
              ]
            initCanvas
            (drawCanvas w h m)
          , div_ [] [ "Remaining meetings: ", text (ms $ show _modelRemMeetings) ]
          , div_ [] [ "Board: ", text _modelBoardName ]
          , div_ [] [ "Player: ", text _modelName ]
          , div_ [] [ "Players:" ]
          , div_ [ CSS.style_ [ CSS.marginLeft "10px" ] ] (map fmtPlayer' _modelPlayers)
          ]

    fmtPlayer' Player{..} =
        div_
            [ CSS.style_ 
                [ CSS.display "flex"
                , CSS.flexWrap "wrap"
                , CSS.verticalAlign "middle"
                , CSS.alignItems "center"
                ]
            ]
            [ div_
                [ CSS.style_ 
                    [ CSS.backgroundColor (CSS.hex _playerCol)
                    , CSS.borderColor backgroundColor
                    , CSS.marginRight "5px" 
                    , CSS.border "5px solid" 
                    , CSS.padding "10px" 
                    ]
                 ]
                [ " " ]
            , text _playerName
            ]

initCanvas :: DOMRef -> Canvas ()
initCanvas _ = pure ()

drawCanvas :: Double -> Double -> Model -> () -> Canvas ()
drawCanvas w h Model{..} () = do
  Canvas.clearRect (0, 0, w, h)

  Canvas.fillStyle (Canvas.color backgroundColor)
  Canvas.fillRect (0, 0, w, h)

  Canvas.fillStyle (Canvas.color wallColor)
  when (_modelBoard /= emptyBoard) $
    forBoard _modelBoard drawBoardCell

  forM_ _modelPlayers $ \Player{..} -> do
    Canvas.fillStyle (Canvas.color (CSS.hex _playerCol))
    drawCell _playerPos

drawBoardCell :: Int -> Int -> Cell -> Canvas ()
drawBoardCell i j = \case
  CellEmpty -> pure ()
  CellWall  -> drawCell (Position i j)

drawCell :: Position -> Canvas ()
drawCell (Position i j) =
  Canvas.fillRect 
    ( cellSizeD * fromIntegral j
    , cellSizeD * fromIntegral i
    , cellSizeD
    , cellSizeD
    )

cellSizeD :: Double
cellSizeD = fromIntegral cellSize

-------------------------------------------------------------------------------
-- Component
-------------------------------------------------------------------------------

appComponent :: App Model Action
appComponent = do
  (component mkModel updateModel viewModel)
    { subs = [ keyboardSub ActionKey ]
    -- , logLevel = DebugAll
    }

