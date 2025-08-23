-- {-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.GameSpec (spec) where

import Data.Either
import Miso
import Test.Hspec
import System.Random

import Game.Game

maze1String :: MisoString
maze1String = 
  "8 10\n" <>
  "xxxxxxxxxx\n" <>
  "x........x\n" <>
  "x........x\n" <>
  "x...xx...x\n" <>
  "x...xx...x\n" <>
  "x........x\n" <>
  "x........x\n" <>
  "xxxxxxxxxx"

maze1String' :: MisoString
maze1String' = 
  "8 10\n" <>
  "xxxxxxxxxx\n" <>
  "x........x\n" <>
  "x........x\n" <>
  "x...xx...x\n" <>
  "x...xx...x\n" <>
  "x........x\n" <>
  "x........x"

mkFail :: String -> SpecWith ()
mkFail msg = describe "Game" $ it msg False

spec :: Spec
spec = do

  let gen = mkStdGen 42

  case loadBoard maze1String (mkGame gen) of

    Left err -> mkFail $ "loadBoard maze1: " <> show err

    Right game1 -> do

      describe "Game, loadBoard" $ do
        it "ok" $ isRight (loadBoard maze1String (mkGame gen))
        it "ko" $ loadBoard maze1String' (mkGame gen) `shouldBe` Left "NoParses TokWall"

      describe "Game, maze1" $ do
        it "isRunning" $ isRunning game1
        it "getPlayers" $ getPlayers game1 `shouldBe` []
        it "getPlayerNames" $ getPlayerNames game1 `shouldBe` []

      describe "Game, maze1" $ do
        it "isRunning" $ isRunning game1
        it "getPlayers" $ getPlayers game1 `shouldBe` []
        it "getPlayerNames" $ getPlayerNames game1 `shouldBe` []

      let eGame2 = tryAddPlayer "foobar" game1
      case eGame2 of
        Left _ -> mkFail "tryAddPlayer game1 foobar"
        Right game2 -> describe "Game, tryAddPlayer foobar game1" $ do
          it "isRunning" $ isRunning game2
          it "getPlayers" $ length (getPlayers game2) `shouldBe` 1
          it "getPlayerNames" $ getPlayerNames game2 `shouldBe` ["foobar"]

      let eGame3 = tryAddPlayer "foobar" =<< eGame2
      case eGame3 of
        Left err -> describe "Game, tryAddPlayer foobar x2" $ do
          it "error" $ err `shouldBe` "foobar is already in the maze"
        Right _ -> mkFail "tryAddPlayer foobar x2"

