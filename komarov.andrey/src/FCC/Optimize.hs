module FCC.Optimize (
  optimize,
  ) where

import FCC.Program

import FCC.Optimize.ShrinkUnused
import FCC.Optimize.CalcPure

import Data.Maybe

optimize :: Program String -> Program String
optimize p = fix' p upd

upd :: Program String -> Program String
upd = shrink . calcSubExprs

fix' :: Eq a => a -> (a -> a) -> a
fix' init mod = if new == init then init else fix' new mod where
  new = mod init
