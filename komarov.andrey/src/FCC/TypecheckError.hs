{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module FCC.TypecheckError (
  TypecheckError(..),
  ) where

import FCC.Type
import FCC.Expr

data TypecheckError where
  SomethingWentWrong :: TypecheckError
  NotCallable :: Show a => Expr a -> TypecheckError
  NotAssignable :: Show a => Expr a -> TypecheckError
  NotAnArray :: Show a => Type -> Expr a -> TypecheckError
  IndexIsNotInt :: Show a => Type -> Expr a -> TypecheckError
  WhileConditionIsNotBool :: Show a => Type -> Expr a -> TypecheckError
  IfConditionIsNotBool :: Show a => Type -> Expr a -> TypecheckError
  UnboundVariable :: Show a => a -> TypecheckError
  EqTypesDiffer :: Show a => Type -> Type -> Expr a -> Expr a -> TypecheckError
  UnsupportedTypeForEq :: Show a => Type -> Expr a -> Expr a -> TypecheckError
  NotAFunction :: Show a => Type -> Expr a -> TypecheckError
  ArgumentsTypesDiffer :: Show a => [Type] -> [Type] -> Expr a -> TypecheckError
  AssignTypeMismatch :: Show a => Type -> Type -> Expr a -> Expr a -> TypecheckError
  WrongReturnType :: Show a => Type -> Type -> Expr a -> TypecheckError

deriving instance Show TypecheckError
