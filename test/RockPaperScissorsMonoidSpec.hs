module RockPaperScissorsMonoidSpec
    (
    spec
    ) where

import           Test.Hspec

import           CommonTypes
import           RockPaperScissorsMonoid
import           System.Random

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Rock player" $
   it "should always play rock" $
    take 5 alwaysRock `shouldMatchList` replicate 5 Rock
  describe "Random player" $ do
   it "should play signs determined by the pseudo number generator" $
    take 5 (randomSigns (mkStdGen 42)) `shouldBe`  [Scissors, Rock, Paper, Paper, Scissors]
   it "should play different signs with a different seed" $
    take 5 (randomSigns (mkStdGen 22555956646546)) `shouldBe`  [Rock,Paper,Scissors,Scissors,Paper]
  describe "Game" $ do
   describe "draws" $ do
    it "should evaluate Rock vs. Rock as draw" $
      play [Rock] [Rock] `shouldBe` draw
    it "should evaluate Paper vs. Paper as draw" $
      play [Paper] [Paper] `shouldBe` draw
    it "should evaluate Paper vs. Paper as draw" $
      play [Scissors] [Scissors] `shouldBe` draw
   describe "first player wins" $ do
    it "should evaluate Rock vs. Scissors -> first player wins" $
      play [Rock] [Scissors] `shouldBe` firstPlayerWins
    it "should evaluate Paper vs. Rock -> first player wins" $
      play [Paper] [Rock] `shouldBe` firstPlayerWins
    it "should evaluate Scissors vs. Paper -> first player wins" $
      play [Scissors] [Paper] `shouldBe` firstPlayerWins
   describe "second player wins" $ do
    it "should evaluate Scissors vs Rock -> second player wins" $
      play  [Scissors][Rock] `shouldBe` secondPlayerWins
    it "should evaluate  Rock vs. Paper -> second player wins" $
      play  [Rock][Paper] `shouldBe` secondPlayerWins
    it "should evaluate Paper vs. Scissors -> second player wins" $
      play  [Paper][Scissors] `shouldBe` secondPlayerWins
   describe "some games" $ do
    it "should evaluate the given game (all signs vs. always Rock)" $
      play [Rock, Paper, Scissors] [Rock,Rock,Rock] `shouldBe` Result 1 1 1
    it "should evaluate the given game (always Rock vs. always Rock)" $
      play [Rock,Rock,Rock] [Rock,Rock,Rock] `shouldBe` Result 0 0 3

draw:: Result
draw = Result 0 0 1

firstPlayerWins:: Result
firstPlayerWins = Result 1 0 0

secondPlayerWins:: Result
secondPlayerWins = Result 0 1 0
