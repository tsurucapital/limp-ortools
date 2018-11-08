module Numeric.Limp.Solvers.OrTools
  ( Status(..)
  , Solver(..)
  , AbnormalDetail(..)
  , solve
  ) where

import qualified Numeric.Limp.Canon as Canon
import qualified Numeric.Limp.Program as Limp
import qualified Numeric.Limp.Rep as Limp

import qualified Numeric.Limp.Solvers.OrTools.Solve as Solve
import Numeric.Limp.Solvers.OrTools.Types

solve
  :: (Ord z, Ord r)
  => Solver
  -> Limp.Program z r Limp.IntDouble
  -> (Status, Limp.Assignment z r Limp.IntDouble)
solve solver = Solve.solve solver . Canon.program
