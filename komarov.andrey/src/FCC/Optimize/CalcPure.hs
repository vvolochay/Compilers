{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FCC.Optimize.CalcPure (

  ) where

import FCC.Expr
import FCC.Eval
import FCC.Program
import FCC.Optimize.StdlibEval

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Set as S
import qualified Data.Map as M

timeout = 10000
impossible = error "FATAL ERROR ¯\\_(ツ)_/¯"

data Context = Context {
  bindings :: M.Map String Value,
  counter :: Int }

newtype Evaluator a = Evaluator {
  runEvaluator :: ExceptT () (StateT Context (Reader (S.Set String))) a
  } deriving (Functor, Applicative, Monad,
              MonadError (), MonadReader (S.Set String), MonadState Context)

tick :: Evaluator ()
tick = do
  remain <- gets counter
  when (remain <= 0) $ throwError ()
  modify $ \c -> c{ counter = remain - 1 }

eval :: Expr String -> Evaluator Value
eval (Var v) = _
eval (Lit i) = return $ VInt i
eval (LitBool b) = return $ VBool b
eval (Lam t s) = _
eval Empty = return $ VVoid
eval (Pop e) = tick >> eval e
eval (Seq e1 e2) = tick >> eval e1 >> eval e2
eval (Call f args) = _
eval (Eq _ _) = impossible
eval (While cond body) = _
eval (If cond thn els) = _
eval (Assign dest src) = _
eval (Array a i) = _
eval (New _) = impossible
eval (Return e) = _

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
