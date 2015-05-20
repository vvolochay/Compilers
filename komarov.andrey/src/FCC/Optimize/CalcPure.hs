{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FCC.Optimize.CalcPure (

  ) where

import FCC.Type
import FCC.Expr
import FCC.Eval
import FCC.Program
import FCC.Optimize.StdlibEval

import Bound

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Set as S
import qualified Data.Map as M

timeout = 10000
impossible = error "FATAL ERROR ¯\\_(ツ)_/¯"

data Context = Context {
  bindings :: M.Map String Value,
  counter :: Int,
  bound :: Int }

data ROContext = ROContext {
  pureFunctions :: S.Set String,
  ctxFunctions :: M.Map String (Function String) }

-- Использовать throwError для сообщения об успехе вместо
               -- какого-нибудь ContT - фу
newtype Evaluator a = Evaluator {
  runEvaluator :: ExceptT (Maybe Value) (StateT Context (Reader ROContext)) a
  } deriving (Functor, Applicative, Monad,
              MonadError (Maybe Value), MonadReader ROContext, MonadState Context)

tick :: Evaluator ()
tick = do
  remain <- gets counter
  when (remain <= 0) $ throwError Nothing
  modify $ \c -> c{ counter = remain - 1 }

fresh :: Evaluator String
fresh = do
  var <- gets bound
  modify $ \c -> c{bound = var + 1}
  return $ "_opt_var_" ++ show var

defaultVal :: Type -> Value
defaultVal TInt = VInt 0
defaultVal TBool = VBool False
defaultVal TVoid = VVoid
defaultVal (TArray _) = VArray M.empty
defaultVal (TFun _ _) = impossible

call :: Function String -> [Value] -> Evaluator Value
call (Function _ _ (Native name _)) args = do
  tick
  case (builtinsE M.! name) args of
   Nothing -> throwError Nothing
   Just res -> return res
call (Function _ fargs (Inner s)) args = do
  _

eval :: Expr String -> Evaluator Value
eval (Var v) = gets $ (M.! v) . bindings
eval (Lit i) = return $ VInt i
eval (LitBool b) = return $ VBool b
eval (Lam t s) = do
  v <- fresh
  modify $ \c -> c{bindings = M.insert v (defaultVal t) (bindings c)}
  eval $ instantiate1 (Var v) s
eval Empty = return $ VVoid
eval (Pop e) = tick >> eval e
eval (Seq e1 e2) = tick >> eval e1 >> eval e2
eval (Call (Var fname) args) = do
  f <- asks $ (M.! fname) . ctxFunctions
  args' <- mapM eval args
  call f args'
eval (Call _ _) = impossible
eval (Eq _ _) = impossible
eval e@(While cond body) = do
  c <- eval cond
  case c of
   VBool b -> if b then eval e else return VVoid
   _ -> impossible
eval (If cond thn els) = do
  c <- eval cond
  case c of
   VBool b -> if b then eval thn else eval els
   _ -> impossible
eval (Assign (Var v) src) = do
  src' <- eval src
  modify $ \c -> c{bindings = M.insert v src' (bindings c)}
  _
eval (Assign (Array a i) src) = _ -- TODO ?????? :((((((
eval (Assign _ _) = impossible
eval (Array a i) = _
eval (New _ _) = impossible
eval (Return e) = do
  e' <- eval e
  return $ e'

findPure :: Program String -> S.Set String
findPure (Program funs vars) = undefined where
  allPure = fix' S.empty (updPure funs)

updPure :: M.Map String (Function String) -> S.Set String -> S.Set String
updPure funs ctx = ctx `S.union` S.fromList [name | (name, f) <- M.toList funs, isPure ctx f]

isPure :: S.Set String -> Function String -> Bool
isPure _ (Function _ _ (Native name _)) = name `M.member` builtinsE
isPure ctx (Function _ _ (Inner s)) = all (`S.member` ctx) s

fix' :: Eq a => a -> (a -> a) -> a
fix' init mod = if new == init then init else fix' new mod where
  new = mod init
