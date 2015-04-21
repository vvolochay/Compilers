{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Typecheck (

  ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error

import qualified AST

data Type
  = TInt
  | TBool
  | TChar
  | TVoid
  | TPointer Type
  | TFun Type [Type]

type Id = String

data Arith = Add | Sub | Mul

data Compare = Less | Greater | LessEq | GreaterEq

data BoolOp = Or | And

data Value = L | R

data CompilationError
  = CompilationError
  deriving (Show)
    
instance Error CompilationError where
  noMsg = CompilationError

data Env = Env

newtype Typechecker a = Typecheker {
  runTypechecker ::
     ErrorT CompilationError (
       State Env) a }
                      deriving (
  Functor, Applicative, Monad, MonadError CompilationError,
  MonadState Env)

class Typecheckable f t | f -> t where
  typecheck :: f -> Typechecker t
