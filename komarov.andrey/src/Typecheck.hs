{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Typecheck (

  ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Except

import AST

data Symbol = Symbol
              deriving (Eq, Ord)

data Env = Env {
               }

data Value = LValue | RValue

data T = T Value Type

data CompilationError
  = CompilationError
  deriving (Show)
    
newtype Typechecker a = Typecheker {
  runTypechecker ::
     ExceptT CompilationError (
       State Env) a }
                      deriving (
  Functor, Applicative, Monad, MonadError CompilationError,
  MonadState Env)

class Typecheckable f t | f -> t where
  typecheck :: f () -> Typechecker (Tagged f t)

{-
data Expression a = EVar Id
                  | ELitInt Int
                  | ELitBool Bool
                  | EArith ArithBinOp (Tagged Expression a) (Tagged Expression a)
                  | EBool BoolBinOp (Tagged Expression a) (Tagged Expression a)
                  | EArithCmp ArithCmpOp (Tagged Expression a) (Tagged Expression a)
                  | EEqual EqOp (Tagged Expression a) (Tagged Expression a)
                  | ECall Id [(Tagged Expression a)]
                  | EAssign (Tagged Expression a) (Tagged Expression a)
                  | EDeref (Tagged Expression a)
                  | EAddr (Tagged Expression a)
                  | EArray (Tagged Expression a) (Tagged Expression a)
                  | ECast Type (Tagged Expression a)
-}

instance Typecheckable Expression T where
  typecheck (EVar var) = _
  typecheck (ELitInt i) = return $ (ELitInt i) `with` (T RValue (Simple "int"))
  typecheck (ELitBool b) = return $ (ELitBool b) `with` (T RValue (Simple "bool"))
