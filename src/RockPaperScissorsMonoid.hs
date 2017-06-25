module RockPaperScissorsMonoid
    (
    alwaysRock,
    randomSigns,
    play
    ) where

import           CommonTypes
import qualified Data.Map.Strict as Map
import           System.Random

play :: [Sign] -> [Sign] -> Result
play a b = accumulateResults $ game a b []

accumulateResults :: [Result] -> Result
accumulateResults = mconcat

game:: [Sign] -> [Sign] -> [Result] -> [Result]
game [] _ result = result
game (x:xs) (y:ys) results =  game xs ys results'
  where results' = gameRound x y : results
game _ _ _ = error "Should not be possible."

gameRound:: Sign -> Sign -> Result
gameRound sA sB | sA == sB = Result 0 0 1
gameRound sA sB | winsAgainst Map.! sA == sB = Result 1 0 0
gameRound sA sB | winsAgainst Map.! sB == sA = Result 0 1 0

winsAgainst :: Map.Map Sign Sign
winsAgainst = Map.fromList[(Rock,Scissors), (Scissors, Paper), (Paper,Rock)]

alwaysRock :: [Sign]
alwaysRock = Rock : alwaysRock

randomSigns::  RandomGen g => g -> [Sign]
randomSigns generator = let (sign, generator') = random generator in
                          sign : randomSigns generator'
