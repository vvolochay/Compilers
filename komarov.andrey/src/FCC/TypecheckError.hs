{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module FCC.TypecheckError (
  TypecheckError(..),
  ) where

import FCC.Expr

data TypecheckError where
  SomethingWentWrong :: TypecheckError
  NotCallable :: Show a => Expr a -> TypecheckError
  NotAssignable :: Show a => Expr a -> TypecheckError

deriving instance Show TypecheckError
