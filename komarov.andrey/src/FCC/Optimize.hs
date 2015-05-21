module FCC.Optimize (
  optimize,
  ) where

import FCC.Program

import FCC.Optimize.ShrinkUnused
--import FCC.Optimize.CalcPure

import Data.Maybe

optimize :: Program String -> Program String
optimize p = fromMaybe p (shrink p)
