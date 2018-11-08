import Control.Applicative
import Control.Arrow (second)
import qualified Data.Map as Map
import Numeric.Limp.Program
  (r, (.+.), lowerR, Constraint(..), conR)
import Numeric.Limp.Rep (R(..), IntDouble)
import qualified Numeric.Limp.Rep as Limp
import qualified Numeric.Limp.Program as Limp
import Numeric.Limp.Solvers.OrTools
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

main :: IO ()
main = defaultMain
  [ testCase "or-tools sample/glop.native" $
      (second unAssignment $ solve GlopNative prog) @?= (Optimal, expected)
  , testCase "or-tools sample/glop.ortools" $
      (second unAssignment $ solve GlopOrTools prog) @?= (Optimal, expected)
  ]

prog :: Limp.Program () Var IntDouble
prog = Limp.Program Limp.Maximise obj constrs bounds
  where
    obj = r X1 10 .+. r X2 6 .+. r X3 4

    constrs
      = r X1 1 .+. r X2 1 .+. r X3 1 :<= conR 100
      :&& r X1 10 .+. r X2 4 .+. r X3 5 :<= conR 600
      :&& r X1 2 .+. r X2 2 .+. r X3 6 :<= conR 300

    bounds =
      [ lowerR 0 X1
      , lowerR 0 X2
      , lowerR 0 X3
      ]

unAssignment
  :: (Ord r, Ord z)
  => Limp.Assignment z r IntDouble
  -> (Map.Map z Int, Map.Map r Double)
unAssignment (Limp.Assignment a b) = (unrwapZ <$> a, Limp.unwrapR <$> b)
  where
    unrwapZ (Limp.Z z) = z

expected :: (Map.Map () Int, Map.Map Var Double)
expected = (,) Map.empty $ Map.fromList
  [ (X1, 33.333333333333336)
  , (X2, 66.66666666666666)
  , (X3, 0)
  ]

data Var = X1 | X2 | X3
  deriving (Eq, Ord, Show)
