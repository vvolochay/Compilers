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
  typecheck 

{-
  = Var a
  | Lit Int
  | LitBool Bool
  | Lam Type (Scope () Expr a)
  | Empty
  | Seq (Expr a) (Expr a)
  | Call (Expr a) [Expr a]
  | Eq (Expr a) (Expr a) -- костыль во имя нереализации ad-hoc полиморфизма
  | While (Expr a) (Expr a)
  | If (Expr a) (Expr a) (Expr a)
  | Assign (Expr a) (Expr a)
  | Array (Expr a) (Expr a)
  | Return (Expr a)
  | Native [String]
-}
