{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FCC.Typecheck (

  ) where

import FCC.Type
import FCC.TypecheckError

import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as M

type Context a = M.Map a Type

newtype Typecheck a = Typecheck {
  runTypecheck :: Except TypecheckError a
  } deriving (Functor, Applicative, Monad, MonadError TypecheckError)

class Typecheckable (f :: * -> *) where
  typecheck :: f a -> Typecheck (f a, Type)
