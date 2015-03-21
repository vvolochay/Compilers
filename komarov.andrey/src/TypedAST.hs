{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs, KindSignatures, DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators, RankNTypes, ImpredicativeTypes #-}
module TypedAST (

  ) where

import Control.Monad.Reader
import Control.Monad.Error
import Control.Applicative
import Data.List (find)

import qualified Data.Map as M

import qualified AST

type Id = String

data Type = TBool
          | TInt
          | TString
          | TVoid
          deriving (Show, Eq)

data Env = Env {
  functions :: M.Map Id (Type, [Type]),
  variables :: M.Map Id Type
  }

newtype Typecheck a = Typecheck { unTypecheck :: ErrorT String (Reader Env) a }
                    deriving (Functor, Applicative, Monad, MonadReader Env, MonadError String)

getType :: Id -> Typecheck Type
getType name = do
  env <- variables <$> ask
  case M.lookup name env of
   Nothing -> throwError $ "No such var in env: " ++ name
   Just t -> return t

getFunType :: Id -> Typecheck (Type, [Type])
getFunType name = do
  funs <- functions <$> ask
  case M.lookup name funs of
   Nothing -> throwError $ "No such fun in env: " ++ show name
   Just t -> return t

addToEnv :: Type -> Id -> Typecheck Env
addToEnv ty id = do
  env <- ask
  let vars = variables env
  case M.lookup id vars of
   Nothing -> return $ env { variables = M.insert id ty vars }
   Just t2 -> throwError $
              "Var " ++ id ++ " : " ++ show t2 ++ " already defined"

addFunsToEnv :: (Id, Type, [Type]) -> Typecheck Env
addFunsToEnv funcs = do
  _

addFunToEnv :: Id -> Type -> [Type] -> Typecheck Env
addFunToEnv name ret args = do
  env <- ask
  let funs = functions env
  case M.lookup name funs of
   Nothing -> return $ env { functions = M.insert name (ret, args) funs }
   Just t -> throwError $
             "Fun " ++ name ++ " : " ++ show t ++ " already defined"

readType :: AST.Type -> Typecheck Type
readType "bool" = return TBool
readType "int" = return TInt
readType "string" = return TString
readType "void" = return TVoid
readType t = throwError $ "Unknown type: " ++ show t

typecheckF :: AST.Program -> Typecheck ()
typecheckF (AST.Program funs) = do
  let types = [(AST.functionName f, AST.functionRetType f, map fst (AST.functionArgs f)) | f <- funs]
  _
  
  

typecheck :: AST.Statement -> Typecheck ()
typecheck (AST.Block stmts) = forM_ stmts typecheck
typecheck (AST.VariableDeclaration ty []) = return ()
typecheck (AST.VariableDeclaration ty (x:xs)) = do
  ty' <- readType ty
  env' <- addToEnv ty' x
  local (\ _ -> env') $ typecheck (AST.VariableDeclaration ty xs)
typecheck (AST.Assignment id e) = do
  tL <- getType id
  tR <- typecheckE e
  if tL /= tR
    then throwError $ "Type mismatch in '(" ++ id ++ " : " ++ show tL ++ ") := (<expr> : " ++ show tR ++ ")'"
    else return ()
typecheck (AST.RawExpression e) = void $ typecheckE e
typecheck (AST.IfThenElse e thn els) = do
  t <- typecheckE e
  when (t /= TBool) $ throwError $ "Type mismatch: condition has type " ++ show t
  typecheck thn
  typecheck els
typecheck (AST.While e stmt) = do
  t <- typecheckE e
  when (t /= TBool) $ throwError $ "Type mismatch: while condition has type " ++ show t
  typecheck stmt
typecheck (AST.Return e) = void $ typecheckE e 

expect :: (Eq a, Show a) => a -> Typecheck a -> Typecheck ()
expect x tx = do
  x' <- tx
  when (x /= x') $ throwError $ "Expected " ++ show x' ++ " to be " ++ show x

typecheckE :: AST.Expression -> Typecheck Type
typecheckE (AST.EVar id) = getType id
typecheckE (AST.EInt i) = return TInt
typecheckE (AST.EBool b) = return TBool
typecheckE (AST.EAdd lhs rhs) = do
  lt <- typecheckE lhs
  rt <- typecheckE rhs
  case (lt, rt) of
   (TInt, TInt) -> return TInt
   (TString, TString) -> return TString
   _ -> throwError $ show lt ++ " + " ++ show rt
typecheckE (AST.ESub lhs rhs) = do
  forM [lhs, rhs] $ expect TInt . typecheckE
  return TInt
typecheckE (AST.EMul lhs rhs) =
  forM [lhs, rhs] (expect TInt . typecheckE) >> return TInt
typecheckE (AST.ELess lhs rhs) = do
  forM [lhs, rhs] (expect TInt . typecheckE) >> return TBool
typecheckE (AST.EGreater lhs rhs) = do
  forM [lhs, rhs] (expect TInt . typecheckE) >> return TBool
typecheckE (AST.ELessEq lhs rhs) = do
  forM [lhs, rhs] (expect TInt . typecheckE) >> return TBool
typecheckE (AST.EGreaterEq lhs rhs) = do
  forM [lhs, rhs] (expect TInt . typecheckE) >> return TBool
typecheckE (AST.EEqual lhs rhs) = do
  lt <- typecheckE lhs
  rt <- typecheckE rhs
  when (lt /= rt) $ throwError $ show lt ++ " == " ++ show rt
  return lt
typecheckE (AST.ENotEqual lhs rhs) = do
  lt <- typecheckE lhs
  rt <- typecheckE rhs
  when (lt /= rt) $ throwError $ show lt ++ " == " ++ show rt
  return lt
typecheckE (AST.EAnd lhs rhs) =
  forM [lhs, rhs] (expect TBool . typecheckE) >> return TBool
typecheckE (AST.EOr lhs rhs) =
  forM [lhs, rhs] (expect TBool . typecheckE) >> return TBool
typecheckE (AST.ECall fun exprs) = do
  (ret, args) <- getFunType fun
  forM (zip args exprs) $ \(t, te) -> do
    e <- typecheckE te
    when (t /= e) $ throwError $ "Fun arg " ++ show t ++ " /= " ++ show e
  return ret

