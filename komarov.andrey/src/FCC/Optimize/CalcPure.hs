{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FCC.Optimize.CalcPure (
  calcSubExprs,
  ) where

import FCC.Eval
import FCC.Expr
import FCC.Program
import FCC.Evaluator

import Bound

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Control.Monad.Reader

import qualified Data.Map as M

calcSubExprs :: Program String -> Program String
calcSubExprs p = runReader (runOptimizer $ optP p) (config p)

newtype Optimizer a = Optimizer {
  runOptimizer :: Reader EvalConfig a
  } deriving (Functor, Applicative, Monad, MonadReader EvalConfig)

optP :: Program String -> Optimizer (Program String)
optP p@(Program funs vars) = do
  funs' <- sequence (fmap optF funs)
  return $ Program funs' vars

optF :: Function String -> Optimizer (Function String)
optF f@(Function _ _ (Native{})) = return f
optF f@(Function args ret (Inner s)) = do
  let names = ["_opt_arg_" ++ show i | (i, _) <- zip [0..] args]
      e = instantiate (Var . (names !!)) s
  e' <- opt e
  let s' = abstract (`elemIndex` names) e'
  return $ Function args ret (Inner s')

opt :: Expr String -> Optimizer (Expr String)
opt e@(Var _) = return e
opt e@(Lit _) = return e
opt e@(LitBool _) = return e
opt Empty = return Empty
opt (Pop e) = Pop <$> opt e
opt (Seq Empty e) = opt e
opt (Seq e Empty) = opt e
opt (Seq e1 e2) = Seq <$> (opt e1) <*> (opt e2)
opt e@(Call (Var fname) args) = do
  cfg <- ask
  let ok = do
        args <- sequence $ map (calc cfg) args
        f <- M.lookup fname (ctxFunctions cfg)
        v <- calcF cfg f args
        v2e v
  case ok of
   Nothing -> return e
   Just e' -> return e'
opt e@(While cond body) = return e
opt e@(If cond thn els) = return e
opt e@(Assign dst src) = return e
opt e@(Array a i) = return e
opt (Return e) = Return <$> opt e
opt e = return e

v2e :: Value -> Maybe (Expr a)
v2e (VInt i) = return $ Lit i
v2e (VBool b) = return $ LitBool b
v2e _ = Nothing
