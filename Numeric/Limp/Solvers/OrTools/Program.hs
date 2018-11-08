{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
module Numeric.Limp.Solvers.OrTools.Program
  ( Program(..)
  , Constraints(..)
  , Linear(..)
  , ConstraintCoefficients(..)
  , Bounds(..)
  , Solution(..)
  , makeProgram
  ) where

import qualified Control.Arrow as Arr
import Data.Either
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector.Storable as VS
import Foreign.Storable
import qualified Numeric.Limp.Canon.Constraint as Canon
import qualified Numeric.Limp.Canon.Linear as Canon
import qualified Numeric.Limp.Canon.Program as Canon
import qualified Numeric.Limp.Rep as Limp

data Program = Program
  { progObjective :: !Linear
  , progConstraints :: !Constraints
  , progVarBounds :: !Bounds
      -- ^ Variable bounds. One entry for each variable.
  , progNIntegerVariables :: !Int
      -- ^ First this many variables are integers, the rest are reals.
  }
  deriving (Show)

data Constraints = Constraints
  { ctrCoeffs :: !ConstraintCoefficients
  , ctrBounds :: !Bounds
  }
  deriving (Show)

data Linear = Linear
  -- All these vectors should be of the same length.
  { linearVariableId :: !(VS.Vector Int)
  , linearCoefficient :: !(VS.Vector Double)
  }
  deriving (Show)

data ConstraintCoefficients = ConstraintCoefficients
  -- All these vectors should be of the same length.
  { ccConstraintId :: !(VS.Vector Int)
  , ccVariableId :: !(VS.Vector Int)
  , ccCoefficient :: !(VS.Vector Double)
  }
  deriving (Show)

data Bounds = Bounds
  -- All these vectors should be of the same length.
  { boundsLower :: !(VS.Vector Double)
  , boundsUpper :: !(VS.Vector Double)
  }
  deriving (Show)

data Solution = Solution
  { solIntegers :: !(VS.Vector Int)
  , solReals :: !(VS.Vector Double)
  }

makeProgram
    :: (Ord z, Ord r)
    => Canon.Program z r Limp.IntDouble
    -> (Program, Solution -> Limp.Assignment z r Limp.IntDouble)
makeProgram prog@Canon.Program
  { _objective = Canon.Linear obj
  , _constraints = Canon.Constraint constrs
  , _bounds = bounds
  }
  = (prog', mkAssignment vars)
  where
    prog' = Program
      { progObjective = obj'
      , progConstraints = constrs'
      , progVarBounds = varBounds
      , progNIntegerVariables = length $ filter isLeft $ Set.toList vars
      }

    obj' = vector2 Linear
      $ map (varToIndex Arr.*** Limp.unwrapR)
      $ Map.toList obj

    constrs' = Constraints
      { ctrCoeffs = vector3 ConstraintCoefficients $ do
          (constraintId, Canon.C1 _ (Canon.Linear linear) _)
            <- zip [0..] constrs
          (var, Limp.R coeff) <- Map.toList linear
          return (constraintId, varToIndex var, coeff)
      , ctrBounds = vector2 Bounds $ do
          Canon.C1 lo _ hi <- constrs
          return $! encodeBounds (lo, hi)
      }

    varBounds = vector2 Bounds $ do
      var <- Set.toList vars
      return $! encodeBounds $ Map.findWithDefault (Nothing, Nothing) var bounds

    encodeBounds (lo, hi) = (lo', hi')
      where
        !lo' = maybe (-inf) Limp.unwrapR lo
        !hi' = maybe inf Limp.unwrapR hi

    varToIndex var = Set.findIndex var vars

    -- The set of all variables. A 'Left' is always smaller than a 'Right', so
    -- all integer vairables appear earlier than all real variables in the set.
    vars = Canon.varsOfProgram prog

    inf = 1 / 0

mkAssignment
  :: (Ord z, Ord r)
  => Set.Set (Either z r) -> Solution -> Limp.Assignment z r Limp.IntDouble
mkAssignment allVars Solution{ solIntegers = ints, solReals = reals }
  = Limp.Assignment (mkMap Limp.Z ints intVars) (mkMap Limp.R reals realVars)
  where
    (intVars, realVars) = partitionEithers $ Set.toList allVars

    mkMap f vec vars = Map.fromList $ zip vars $ map f $ VS.toList vec

vector2
    :: (Storable a, Storable b)
    => (VS.Vector a -> VS.Vector b -> r)
    -> [(a, b)]
    -> r
vector2 cont (unzip -> (as, bs)) = cont (VS.fromList as) (VS.fromList bs)
{-# INLINE vector2 #-}

vector3
    :: (Storable a, Storable b, Storable c)
    => (VS.Vector a -> VS.Vector b -> VS.Vector c -> r)
    -> [(a, b, c)]
    -> r
vector3 cont (unzip3 -> (as, bs, cs))
  = cont (VS.fromList as) (VS.fromList bs) (VS.fromList cs)
{-# INLINE vector3 #-}
