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
import Control.Monad.Cont

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
  ctxFunctions :: M.Map String (Function String)
  }

-- Использовать throwError для сообщения об успехе вместо
               -- какого-нибудь ContT - фу
newtype Evaluator r a = Evaluator {
  runEvaluator :: ExceptT (Maybe Value) (ContT r (StateT Context (Reader ROContext))) a
  } deriving (Functor, Applicative, Monad,
              MonadError (Maybe Value), MonadReader ROContext,
              MonadState Context, MonadCont)

tick :: Evaluator r ()
tick = do
  remain <- gets counter
  when (remain <= 0) $ throwError Nothing
  modify $ \c -> c{ counter = remain - 1 }

fresh :: Evaluator r String
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

{-
call :: Function String -> [Value] -> Evaluator Value
call (Function _ _ (Native name _)) args = do
  tick
  case (builtinsE M.! name) args of
   Nothing -> throwError Nothing
   Just res -> return res
call (Function _ fargs (Inner s)) args = do
  _
-}

call :: _
call = _

eval :: _ -> Expr String -> Evaluator r Value
eval k (Var v) = gets $ (M.! v) . bindings
eval k (Lit i) = return $ VInt i
eval k (LitBool b) = return $ VBool b
eval k (Lam t s) = do
  v <- fresh
  modify $ \c -> c{bindings = M.insert v (defaultVal t) (bindings c)}
  eval k $ instantiate1 (Var v) s
eval k Empty = return $ VVoid
eval k (Pop e) = tick >> eval k e
eval k (Seq e1 e2) = tick >> eval k e1 >> eval k e2
eval k (Call (Var fname) args) = do
  f <- asks $ (M.! fname) . ctxFunctions
  args' <- mapM (eval k) args
  call f args'
eval k (Call _ _) = impossible
eval k (Eq _ _) = impossible
eval k e@(While cond body) = do
  c <- eval k cond
  case c of
   VBool b -> if b then eval k e else return VVoid
   _ -> impossible
eval k (If cond thn els) = do
  c <- eval k cond
  case c of
   VBool b -> if b then eval k thn else eval k els
   _ -> impossible
eval k (Assign (Var v) src) = do
  src' <- eval k src
  modify $ \c -> c{bindings = M.insert v src' (bindings c)}
  _
eval k (Assign (Array a i) src) = _ -- TODO ?????? :((((((
eval k (Assign _ _) = impossible
eval k (Array a i) = _
eval k (New _ _) = impossible
eval k (Return e) = do
  e' <- eval k e
  k e'

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
