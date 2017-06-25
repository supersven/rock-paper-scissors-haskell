module Main where

import           CommonTypes
import           Control.Monad.State
import           RockPaperScissorsMonoid     as RPSMonoid
import           RockPaperScissorsStateMonad as RPSStateMonad
import           System.Random

main :: IO ()
main = do
  print $ RPSMonoid.play (take 100 RPSMonoid.alwaysRock) (take 100 $ randomSigns (mkStdGen 46168749618168))
  print $ evalState (RPSStateMonad.playTimes 100) (Rock, mkStdGen 46168749618168, Result 0 0 0)
  return ()
