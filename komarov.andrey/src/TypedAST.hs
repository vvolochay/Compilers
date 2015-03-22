{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
module TypedAST (

  ) where

import Control.Monad.Reader
import Control.Monad.Error
import Control.Applicative

import qualified Data.Map as M

import qualified AST

type Id = String

data Type = TBool
          | TInt
          | TString
          | TVoid
          deriving (Show, Eq)

data FType = FType Type [Type]

data Env = Env {
  functions :: M.Map Id FType,
  variables :: M.Map Id Type
  }

data TypecheckError = UnknownVar Id
                    | UnknownFun Id
                    | SomethingWentWrong String
                    | TypeMismatch Type Type
                    deriving (Show)

instance Error TypecheckError where
  strMsg = SomethingWentWrong 

newtype Typechecker a = Typechecker { unCompiler :: ErrorT TypecheckError (Reader Env) a } deriving (Functor, Applicative, Monad, MonadReader Env, MonadError TypecheckError)

class Typecheckable a b | a -> b where
  typecheck :: a -> Typechecker b

getVarType :: Id -> Typechecker Type
getVarType name = do
  vars <- variables <$> ask
  case M.lookup name vars of
   Nothing -> throwError $ UnknownVar name
   Just t -> return t

getFunType :: Id -> Typechecker FType
getFunType name = do
  funs <- functions <$> ask
  case M.lookup name funs of
   Nothing -> throwError $ UnknownFun name
   Just t -> return t

expect :: Typecheckable t Type => Type -> t -> Typechecker ()
expect x ta = do
  a <- typecheck ta
  when (a /= x) $ throwError $ TypeMismatch a x

instance Typecheckable AST.Expression Type where
  typecheck (AST.EVar v) = getVarType v
  typecheck (AST.EInt _) = return TInt
  typecheck (AST.EBool _) = return TBool
  typecheck (AST.EAdd lhs rhs) =
    expect TInt lhs >> expect TInt rhs >> return TInt
  typecheck (AST.ESub lhs rhs) =
    expect TInt lhs >> expect TInt rhs >> return TInt
  typecheck (AST.EMul lhs rhs) =
    expect TInt lhs >> expect TInt rhs >> return TInt
  typecheck (AST.ELess lhs rhs) =
    expect TInt lhs >> expect TInt rhs >> return TBool
  typecheck (AST.EGreater lhs rhs) =
    expect TInt lhs >> expect TInt rhs >> return TBool
  typecheck (AST.ELessEq lhs rhs) =
    expect TInt lhs >> expect TInt rhs >> return TBool
  typecheck (AST.EGreaterEq lhs rhs) =
    expect TInt lhs >> expect TInt rhs >> return TBool
  typecheck (AST.EEqual lhs rhs) = do
    tl <- typecheck lhs
    tr <- typecheck rhs
    when (tl /= tr) $ throwError $ TypeMismatch tl tr
    return TBool
  typecheck (AST.ENotEqual lhs rhs) = do
    tl <- typecheck lhs
    tr <- typecheck rhs
    when (tl /= tr) $ throwError $ TypeMismatch tl tr
    return TBool
  typecheck (AST.EAnd lhs rhs) =
    expect TBool lhs >> expect TBool rhs >> return TBool
  typecheck (AST.EOr lhs rhs) =
    expect TBool lhs >> expect TBool rhs >> return TBool
  typecheck (AST.ECall name args) = do
    targs <- forM args typecheck
    FType ret targs' <- getFunType name
    forM (zip targs targs') $ \(t, t') ->
      when (t /= t') $ throwError $ TypeMismatch t t'
    return ret
