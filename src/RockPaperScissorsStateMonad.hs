module RockPaperScissorsStateMonad
    (
    alwaysRock,
    play,
    randomSign,
    Player(..),
    playTimes
    ) where

import           Control.Monad.Loops
import qualified Control.Monad.State as SM
import qualified Data.Map.Strict     as Map
import           Debug.Trace
import           System.Random
import CommonTypes

class Player s where
  bet :: s -> (Sign, s)

instance Player StdGen where
  bet s = randomSign s

instance Player Sign where
  bet s = (s,s)

playTimes :: (Player a, Player b) => Int ->  SM.State (a, b, Result) Result
playTimes n = iterateWhile (\r -> gameCount r < n) play

gameCount :: Result -> Int
gameCount r = winCountA r + winCountB r + draws r

play :: (Player a, Player b) => SM.State (a, b, Result) Result
play = do
        (p1, p2, prevResult) <- SM.get
        let (sign1, state1) = bet p1
        let (sign2, state2) = bet p2
        let result = gameRound sign1 sign2
        SM.put (state1, state2, mappend prevResult result)
        return $ mappend prevResult result

gameRound:: Sign -> Sign -> Result
gameRound sA sB | sA == sB = Result 0 0 1
gameRound sA sB | winsAgainst Map.! sA == sB = Result 1 0 0
gameRound sA sB | winsAgainst Map.! sB == sA = Result 0 1 0

winsAgainst :: Map.Map Sign Sign
winsAgainst = Map.fromList[(Rock,Scissors), (Scissors, Paper), (Paper,Rock)]

alwaysRock :: Sign
alwaysRock = Rock

randomSign::  RandomGen g => g -> (Sign,g)
randomSign generator = random generator
