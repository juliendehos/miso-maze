{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Game.Game where

import Data.Map qualified as M
import Data.Vector qualified as V
import Miso hiding (go)
import Miso.Lens
-- import Miso.Lens.TH
import System.Random

import Game.Common

-------------------------------------------------------------------------------
-- params
-------------------------------------------------------------------------------

playerColors :: [Int]
playerColors =
  [ 0xFF0000
  , 0xFFFF00
  , 0xFFFFFF
  , 0xFF00FF
  ]

nbMeetings :: Int
nbMeetings = 5

-------------------------------------------------------------------------------
-- types
-------------------------------------------------------------------------------

data InfoPlayer = InfoPlayer
  { _infoPos :: Position
  , _infoCol :: Int
  } deriving (Eq, Show)

type MapInfoPlayer = M.Map MisoString InfoPlayer

data Game = Game
  { _gameBoard :: Board
  , _gameInfoPlayer :: MapInfoPlayer
  , _gameFreeColors :: [Int]
  , _gameRemMeetings :: Int
  , _gameGen :: StdGen
  } deriving (Eq, Show)

-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

mkGame :: StdGen -> Game
mkGame = Game emptyBoard M.empty playerColors nbMeetings

tryAddPlayer :: MisoString -> Game -> Either MisoString Game
tryAddPlayer name g@Game{..} =
  case _gameFreeColors of
    [] -> Left "the maze is full"
    (c:cs) -> 
      if M.member name _gameInfoPlayer
        then Left $ name <> " is already in the maze"
        else Right $ g & gameFreeColors .~ cs
                       & gameInfoPlayer %~ M.insert name (InfoPlayer pos c)
                       & gameGen .~ gen
  where
    (pos, gen) = genPosition _gameBoard _gameInfoPlayer _gameGen

deletePlayer :: MisoString -> Game -> Game
deletePlayer name g@Game{..} =
  case M.lookup name _gameInfoPlayer of
    Nothing -> g
    Just info -> 
      g & gameInfoPlayer %~ M.delete name       -- remove the player
        & gameFreeColors %~ (_infoCol info :)   -- free the player color

loadBoard :: MisoString -> Game -> Either MisoString Game
loadBoard str g = toGame <$> parseBoard str
  where
    toGame b = g & gameBoard .~ b
                 & gameRemMeetings .~ nbMeetings
                 & resetPositions

playMove :: MisoString -> Move -> Game -> Maybe Game
playMove name move g =
  case (M.lookup name (_gameInfoPlayer g), isRunning g) of
    (Just info, True) -> 
      case move of
        MoveLeft  -> go info $ Position 0 (-1)
        MoveRight -> go info $ Position 0 1
        MoveUp    -> go info $ Position (-1) 0
        MoveDown  -> go info $ Position 1 0
    _ -> Nothing

  where
    go :: InfoPlayer -> Position -> Maybe Game
    go info dij = 
      case tryMove (_gameBoard g) (_infoPos info) dij of
        Nothing -> Nothing
        Just ij1 ->
          if isMeeting name ij1 g
            then Just $ g & gameRemMeetings %~ subtract 1
                          & resetPositions
            else Just $ g & gameInfoPlayer %~ M.adjust (\i -> i & infoPos .~ ij1) name

isRunning :: Game -> Bool
isRunning Game{..} = _gameRemMeetings > 0

getPlayers :: Game -> [Player]
getPlayers Game{..} = M.foldrWithKey' f [] _gameInfoPlayer
  where
    f name (InfoPlayer pos col) acc = Player name col pos : acc

getPlayerNames :: Game -> [MisoString]
getPlayerNames = M.keys . _gameInfoPlayer

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

tryMove :: Board -> Position -> Position -> Maybe Position
tryMove Board{..} (Position i0 j0) (Position di dj) =
  if c == CellEmpty then Just ij1 else Nothing
  where
    (ni, nj) = _boardNiNj
    i1 = (i0 + di) `mod` ni
    j1 = (j0 + dj) `mod` nj
    ij1 = Position i1 j1
    c = _boardData V.! ij2k nj (i1, j1)

isMeeting :: MisoString -> Position -> Game -> Bool
isMeeting name ij1 Game{..} = M.foldrWithKey' f False _gameInfoPlayer
  where
    f n (InfoPlayer ij _) acc = acc || n/=name && ij==ij1

-- generate a free random position, using rejection sampling
genPosition :: Board -> MapInfoPlayer -> StdGen -> (Position, StdGen)
genPosition Board{..} infos = go 
  where
    (ni, nj) = _boardNiNj
    playerPositions = M.foldl' (\acc i -> _infoPos i : acc) [] infos
    go gen = 
      let (pos, gen') = uniformR (Position 0 0, Position (ni-1) (nj-1)) gen
      in if _boardData V.! pos2k nj pos == CellEmpty && notElem pos playerPositions
        then (pos, gen')
        else go gen'

resetPositions :: Game -> Game
resetPositions g@Game{..} =
  g & gameInfoPlayer .~ infos1
    & gameGen .~ gen1
  where
    fAcc (infos, gen) n i = 
      let (pos, gen') = genPosition _gameBoard infos gen
      in (M.insert n (i & infoPos .~ pos) infos, gen')
    (infos1, gen1) = M.foldlWithKey' fAcc (M.empty, _gameGen) _gameInfoPlayer

-------------------------------------------------------------------------------
-- lenses
-------------------------------------------------------------------------------

-- makeLenses ''InfoPlayer

infoPos :: Lens InfoPlayer Position
infoPos = lens _infoPos (\ record field -> record {_infoPos = field})

infoCol :: Lens InfoPlayer Int
infoCol = lens _infoCol (\ record field -> record {_infoCol = field})

-- makeLenses ''Game

gameBoard :: Lens Game Board
gameBoard = lens _gameBoard (\ record field -> record {_gameBoard = field})

gameInfoPlayer :: Lens Game MapInfoPlayer
gameInfoPlayer = lens _gameInfoPlayer (\ record field -> record {_gameInfoPlayer = field})

gameFreeColors :: Lens Game [Int]
gameFreeColors = lens _gameFreeColors (\ record field -> record {_gameFreeColors = field})

gameRemMeetings :: Lens Game Int
gameRemMeetings = lens _gameRemMeetings (\ record field -> record {_gameRemMeetings = field})

gameGen :: Lens Game StdGen
gameGen = lens _gameGen (\ record field -> record {_gameGen = field})

