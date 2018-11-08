{-# LANGUAGE BangPatterns #-}
module Numeric.Limp.Solvers.OrTools.Solve
  ( Status(..)
  , AbnormalDetail(..)
  , solve
  ) where

import Control.Applicative
import qualified Numeric.Limp.Canon as Canon
import qualified Numeric.Limp.Rep as Limp

import Numeric.Limp.Solvers.OrTools.FFI
import Numeric.Limp.Solvers.OrTools.Program
import Numeric.Limp.Solvers.OrTools.Types

solve
  :: (Ord z, Ord r)
  => Solver
  -> Canon.Program z r Limp.IntDouble
  -> (Status, Limp.Assignment z r Limp.IntDouble)
solve solver prog = mkAssignment <$> solveFFI solver prog'
  where
    !(prog', mkAssignment) = makeProgram prog
