module RockPaperScissorsStateMonadSpec
    (
    spec
    ) where

import           Test.Hspec

import           CommonTypes
import           Control.Monad.State.Lazy
import           RockPaperScissorsStateMonad
import           System.Random

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Rock player" $
   it "should always play rock" $
    alwaysRock `shouldBe` Rock
  describe "Random player" $ do
   it "should play signs determined by the pseudo number generator" $
    randomSign (mkStdGen scissorsSeed) `shouldSatisfy` signIs Scissors
   it "should play different signs with a different seed" $
    randomSign (mkStdGen rockSeed) `shouldSatisfy` signIs Rock
   it "should play different signs with a different seed" $
    randomSign (mkStdGen paperSeed) `shouldSatisfy` signIs Paper
  describe "Game" $ do
   describe "draws" $ do
    it "should evaluate Rock vs. Rock as draw" $
      evalState play (mkStdGen rockSeed, Rock, initialresult) `shouldBe` draw
    it "should evaluate Paper vs. Paper as draw" $
      evalState play  (mkStdGen paperSeed, mkStdGen paperSeed, initialresult) `shouldBe` draw
    it "should evaluate Scissors vs. Scissors as draw" $
      evalState play (Scissors, Scissors, initialresult) `shouldBe` draw

   describe "first player wins" $ do
    it "should evaluate Rock vs. Scissors -> first player wins" $
      evalState play (Rock, Scissors, initialresult) `shouldBe` firstPlayerWins
    it "should evaluate Paper vs. Rock -> first player wins" $
      evalState play (Paper, Rock, initialresult) `shouldBe` firstPlayerWins
    it "should evaluate Scissors vs. Paper -> first player wins" $
      evalState play (Scissors, Paper, initialresult) `shouldBe` firstPlayerWins

   describe "second player wins" $ do
    it "should evaluate Scissors vs Rock -> second player wins" $
      evalState play (Scissors, Rock, initialresult) `shouldBe` secondPlayerWins
    it "should evaluate  Rock vs. Paper -> second player wins" $
      evalState play (Rock, Paper, initialresult) `shouldBe` secondPlayerWins
    it "should evaluate Paper vs. Scissors -> second player wins" $
      evalState play (Paper, Scissors, initialresult) `shouldBe` secondPlayerWins

   describe "some games" $ do
     it "should play three rounds Rock vs Rock" $
      evalState (playTimes 3) (Rock, Rock, initialresult) `shouldBe` Result 0 0 3
     it "should play three rounds Rock vs Paper" $
      evalState (playTimes 3) (Rock, Paper, initialresult) `shouldBe` Result 0 3 0

draw:: Result
draw = Result 0 0 1

initialresult = Result 0 0 0

firstPlayerWins:: Result
firstPlayerWins = Result 1 0 0

secondPlayerWins:: Result
secondPlayerWins = Result 0 1 0

signIs :: RandomGen g => Sign ->  (Sign, g) -> Bool
signIs sign resultTupel = fst resultTupel == sign

paperSeed = 131313213

rockSeed = 22555956646546

scissorsSeed = 42
