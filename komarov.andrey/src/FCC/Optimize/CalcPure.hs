{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FCC.Optimize.CalcPure (

  ) where

import FCC.Expr
import FCC.Program
import FCC.Optimize.StdlibEval
import FCC.Evaluator

import Bound

import Data.Maybe (fromMaybe)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Cont

optP :: Program String -> Maybe (Program String)
optP p@(Program funs vars) = Nothing where

optF :: Function String -> Function String
optF f@(Function _ _ (Native{})) = f
optF f@(Function args _ (Inner s)) = f

opt :: Expr String -> Expr String
opt e@(Var _) = e
opt e@(Lit _) = e
opt e@(LitBool _) = e
opt Empty = Empty
opt (Pop e) = Pop $ opt e
opt (Seq e1 e2) = Seq (opt e1) (opt e2)
opt e@(Call f args) = e
opt e@(While cond body) = e
opt e@(If cond thn els) = e
opt e@(Assign dst src) = e
opt e@(Array a i) = e
opt (Return e) = Return $ opt e
opt e = e

