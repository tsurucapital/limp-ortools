{-# LANGUAGE DeriveDataTypeable #-}
module Numeric.Limp.Solvers.OrTools.Types
  ( Solver(..)
  , Status(..)
  , AbnormalDetail(..)
  ) where

import Data.Typeable

data Solver
  = GlopOrTools
  | GlopNative
  deriving (Show, Eq, Ord, Typeable)

data Status
  = Optimal
  | Infeasible
  | Unbounded
  | Abnormal !AbnormalDetail
  deriving (Show, Eq, Ord, Typeable)

data AbnormalDetail
  = AbnormalGlopImprecise
  | AbnormalInvalidProblem
  | AbnormalUnknown
  deriving (Show, Eq, Ord, Typeable)
