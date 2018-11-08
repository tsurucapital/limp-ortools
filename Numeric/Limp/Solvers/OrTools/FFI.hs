{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Numeric.Limp.Solvers.OrTools.FFI (solveFFI) where

import Control.Applicative
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

import Numeric.Limp.Solvers.OrTools.Program
import Numeric.Limp.Solvers.OrTools.Types

solveFFI :: Solver -> Program -> (Status, Solution)
solveFFI solver program = unsafePerformIO $ solveIO solver program

solveIO :: Solver -> Program -> IO (Status, Solution)
solveIO solver
  Program
    { progObjective = obj
    , progConstraints = ctr
    , progVarBounds = bounds
    , progNIntegerVariables = nints
    } =
  VS.unsafeWith (linearVariableId obj) $ \pObjVars ->
  VS.unsafeWith (linearCoefficient obj) $ \pObjCoeffs ->
  VS.unsafeWith (ccConstraintId $ ctrCoeffs ctr) $ \pCtrIds ->
  VS.unsafeWith (ccVariableId $ ctrCoeffs ctr) $ \pCtrVars ->
  VS.unsafeWith (ccCoefficient $ ctrCoeffs ctr) $ \pCtrCoeffs ->
  VS.unsafeWith (boundsLower $ ctrBounds ctr) $ \pCtrLower ->
  VS.unsafeWith (boundsUpper $ ctrBounds ctr) $ \pCtrUpper ->
  VS.unsafeWith (boundsLower bounds) $ \pVarLower ->
  VS.unsafeWith (boundsUpper bounds) $ \pVarUpper ->
  withNewVec nints $ \vecSolInts pSolInts ->
  withNewVec nreals $ \vecSolReals pSolReals -> do

    statusInt <- solve
      nints nreals nctrs nObjTerms nCtrTerms
      pObjVars pObjCoeffs pCtrIds pCtrVars pCtrCoeffs pCtrLower pCtrUpper
      pVarLower pVarUpper
      pSolInts pSolReals

    sol <- Solution
      <$> VS.unsafeFreeze vecSolInts
      <*> VS.unsafeFreeze vecSolReals
    let
      !status = case statusInt of
        0 -> Optimal
        1 -> Infeasible
        2 -> Unbounded
        3 -> Abnormal AbnormalUnknown
        4 -> Abnormal AbnormalGlopImprecise
        5 -> Abnormal AbnormalInvalidProblem
        _ -> error $ "Numeric.Limp.Solvers.OrTools.FFI.solveIO: bad return value: "
              ++ show statusInt
    return (status, sol)
  where
    !nreals = VS.length (boundsLower bounds) - nints
    !nctrs = VS.length (boundsLower $ ctrBounds ctr)
    !nObjTerms = VS.length (linearVariableId obj)
    !nCtrTerms = VS.length (ccConstraintId $ ctrCoeffs ctr)

    solve = case solver of
      GlopOrTools -> c_hs_ortools_solve_mip
      GlopNative -> c_hs_glop_solve_mip

withNewVec :: (Storable a) => Int -> (VSM.IOVector a -> Ptr a -> IO r) -> IO r
withNewVec size body = do
  vec <- VSM.new size
  VSM.unsafeWith vec $ body vec

foreign import ccall "hs_ortools_solve_mip"
  c_hs_ortools_solve_mip
    :: Int -> Int -> Int -> Int -> Int
    -> Ptr Int -> Ptr Double
    -> Ptr Int -> Ptr Int -> Ptr Double
    -> Ptr Double -> Ptr Double
    -> Ptr Double -> Ptr Double
    -> Ptr Int -> Ptr Double
    -> IO Int

foreign import ccall "hs_glop_solve_mip"
  c_hs_glop_solve_mip
    :: Int -> Int -> Int -> Int -> Int
    -> Ptr Int -> Ptr Double
    -> Ptr Int -> Ptr Int -> Ptr Double
    -> Ptr Double -> Ptr Double
    -> Ptr Double -> Ptr Double
    -> Ptr Int -> Ptr Double
    -> IO Int
