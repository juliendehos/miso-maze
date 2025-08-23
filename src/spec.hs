
import Test.Hspec

import Game.CommonSpec
import Game.GameSpec

main :: IO ()
main = hspec $ do
  Game.CommonSpec.spec
  Game.GameSpec.spec

