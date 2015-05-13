{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FCC.Typecheck (

  ) where

import FCC.Type
import FCC.TypecheckError
import FCC.Expr

import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as M

type Context a = M.Map a Type

newtype Typecheck a = Typecheck {
  runTypecheck :: Except TypecheckError a
  } deriving (Functor, Applicative, Monad, MonadError TypecheckError)

class Typecheckable (f :: * -> *) where
  typecheck :: f a -> Typecheck (f a, Type)

instance Typecheckable Expr where
  typecheck (Var v) = _
  typecheck (Lit i) = return (Lit i, TInt)
  typecheck (LitBool b) = return (LitBool b, TBool)
  typecheck (Lam t s) = _
  typecheck Empty = return (Empty, TVoid)
  typecheck (Seq e1 e2) = do
    (e1', _) <- typecheck e1
    (e2', _) <- typecheck e2
    return $ (Seq e1' e2', TVoid)
  typecheck (Call (Var name) args) = _
  typecheck (Call f args) = throwError $ NotCallable f
  typecheck (Eq e1 e2) = _
  typecheck (While cond body) = _
  typecheck (If cond thn els) = _
  typecheck (Assign (Var v) val) = _
  typecheck (Assign (Array a i) val) = _
  typecheck (Assign dst val) = throwError $ NotAssignable dst
  typecheck (Array a i) = _
  typecheck (Return e) = _

