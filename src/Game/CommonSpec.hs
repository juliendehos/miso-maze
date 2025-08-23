-- {-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.CommonSpec (spec) where

import Data.Either
import Data.Vector qualified as V
import Miso
import Miso.Util.Lexer as L
import Miso.Util.Parser as P
import Test.Hspec

import Game.Common

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

maze1Tokens :: [Token]
maze1Tokens =
  [ TokNb 8
  , TokNb 10
  , TokWall, TokWall,  TokWall,  TokWall,  TokWall,  TokWall,  TokWall,  TokWall,  TokWall,  TokWall
  , TokWall, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokWall
  , TokWall, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokWall
  , TokWall, TokEmpty, TokEmpty, TokEmpty, TokWall,  TokWall,  TokEmpty, TokEmpty, TokEmpty, TokWall
  , TokWall, TokEmpty, TokEmpty, TokEmpty, TokWall,  TokWall,  TokEmpty, TokEmpty, TokEmpty, TokWall
  , TokWall, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokWall
  , TokWall, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokWall
  , TokWall, TokWall,  TokWall,  TokWall,  TokWall,  TokWall,  TokWall,  TokWall,  TokWall,  TokWall
  ]

maze1Tokens' :: [Token]
maze1Tokens' =
  [ TokNb 8
  , TokNb 10
  , TokWall, TokWall,  TokWall,  TokWall,  TokWall,  TokWall,  TokWall,  TokWall,  TokWall,  TokWall
  , TokWall, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokWall
  , TokWall, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokWall
  , TokWall, TokEmpty, TokEmpty, TokEmpty, TokWall,  TokWall,  TokEmpty, TokEmpty, TokEmpty, TokWall
  , TokWall, TokEmpty, TokEmpty, TokEmpty, TokWall,  TokWall,  TokEmpty, TokEmpty, TokEmpty, TokWall
  , TokWall, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokWall
  , TokWall, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokEmpty, TokWall
  ]

maze1Board :: Board
maze1Board = Board (8, 10) $ V.fromList
  [ CellWall, CellWall,  CellWall,  CellWall,  CellWall,  CellWall,  CellWall,  CellWall,  CellWall,  CellWall 
  , CellWall, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellWall 
  , CellWall, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellWall 
  , CellWall, CellEmpty, CellEmpty, CellEmpty, CellWall,  CellWall,  CellEmpty, CellEmpty, CellEmpty, CellWall 
  , CellWall, CellEmpty, CellEmpty, CellEmpty, CellWall,  CellWall,  CellEmpty, CellEmpty, CellEmpty, CellWall 
  , CellWall, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellWall 
  , CellWall, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellEmpty, CellWall 
  , CellWall, CellWall,  CellWall,  CellWall,  CellWall,  CellWall,  CellWall,  CellWall,  CellWall,  CellWall
  ]

spec :: Spec
spec = do

  describe "Common, lexer" $ do
    it "nb 8" $ fmap fst (runLexer lexerToken (mkStream "8")) `shouldBe` Right (TokNb 8)
    it "nb 10" $ fmap fst (runLexer lexerToken (mkStream "10")) `shouldBe` Right (TokNb 10)
    it "nb foobar" $ isLeft (runLexer lexerToken (mkStream "foobar"))
    it "wall" $ fmap fst (runLexer lexerToken (mkStream "x")) `shouldBe` Right TokWall
    it "empty" $ fmap fst (runLexer lexerToken (mkStream ".")) `shouldBe` Right TokEmpty
    it "maze1" $ fmap fst (runLexer lexerTokens (mkStream maze1String)) `shouldBe` Right maze1Tokens

  describe "Common, parser" $ do
    it "maze1 ok" $ parse parserBoard maze1Tokens `shouldBe` Right maze1Board
    it "maze1 ko" $ isLeft (parse parserBoard maze1Tokens')

  describe "Common, parseBoard" $ do
    it "maze1 ok" $ parseBoard maze1String `shouldBe` Right maze1Board
    it "maze1 ko" $ parseBoard maze1String' `shouldBe` Left "NoParses TokWall"

