module CommonTypes
    (
    Sign(..),
    Result(..)
    ) where
import           System.Random

data Sign = Rock | Paper | Scissors
  deriving (Enum,Bounded,Eq,Show,Ord)

instance Random Sign where
  randomR (minSign,maxSign) generator =
      let (sign, generator') = randomR (fromEnum minSign, fromEnum maxSign) generator in
              (toEnum sign, generator')
  random = randomR (Rock, Scissors)

data Result = Result {
                    winCountA:: Int,
                    winCountB:: Int,
                    draws:: Int
                  } deriving (Show, Eq)

instance Monoid Result where
  mempty = Result 0 0 0
  mappend x y = Result (winCountA x + winCountA y) (winCountB x + winCountB y) (draws x + draws y)
