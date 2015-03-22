{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
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
  functions :: M.Map Id (Type, [Type]),
  variables :: M.Map Id Type
  }

newtype Typechecker a = Typechecker { unCompiler :: ErrorT String (Reader Env) a } deriving (Functor, Applicative, Monad, MonadReader Env, MonadError String)

class Typecheckable a b | a -> b where
  typecheck :: a -> Typechecker b

instance Typecheckable AST.Expression Type where
  typecheck _ = _
