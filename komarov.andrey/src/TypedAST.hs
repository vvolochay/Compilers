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

type Env = M.Map Id Type

newtype Typecheck a = Typecheck { unTypecheck :: ErrorT String (Reader Env) a }
                    deriving (Functor, Applicative, Monad, MonadReader Env, MonadError String)

getType :: Id -> Typecheck Type
getType name = do
  env <- ask
  case M.lookup name env of
   Nothing -> throwError $ "No such var in env: " ++ name
   Just t -> return $ t

addToEnv :: Type -> Id -> Typecheck Env
addToEnv ty id = do
  env <- ask
  case M.lookup id env of
   Nothing -> return $ M.insert id ty env
   Just t2 -> throwError $
              "Var " ++ id ++ " : " ++ show t2 ++ " already defined"

readType :: AST.Type -> Typecheck Type
readType "bool" = return TBool
readType "int" = return TInt
readType "string" = return TString
readType "void" = return TVoid
readType t = throwError $ "Unknown type: " ++ show t

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
  lt <- typecheckE lhs
  when (lt /= TInt) $ throwError $ "lhs of - : " ++ show lhs
  rt <- typecheckE rhs
  when (rt /= TInt) $ throwError $ "rhs of - : " ++ show rhs
  return TInt
typecheckE (AST.EMul lhs rhs) = do
  lt <- typecheckE lhs
  when (lt /= TInt) $ throwError $ "lhs of * : " ++ show lhs
  rt <- typecheckE rhs
  when (rt /= TInt) $ throwError $ "rhs of * : " ++ show rhs
  return TInt
typecheckE (AST.ELess lhs rhs) = do
  lt <- typecheckE lhs
  when (lt /= TInt) $ throwError $ "lhs of < : " ++ show lhs
  rt <- typecheckE rhs
  when (rt /= TInt) $ throwError $ "rhs of < : " ++ show rhs
  return TBool
typecheckE _ = _
