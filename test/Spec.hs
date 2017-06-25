import           Test.Hspec

import qualified RockPaperScissorsMonoidSpec
import qualified RockPaperScissorsStateMonadSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "RockPaperScissorsMonoid"     RockPaperScissorsMonoidSpec.spec
  describe "RockPaperScissorsStateMonad"     RockPaperScissorsStateMonadSpec.spec
