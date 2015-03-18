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

type Env = [(Id, Type)]

newtype Typecheck a = Typecheck { unTypecheck :: ErrorT String (Reader Env) a }
                    deriving (Functor, Applicative, Monad, MonadReader Env, MonadError String)

getType :: Id -> Typecheck Type
getType name = do
  env <- ask
  case find ((== name) . fst) env of
   Nothing -> throwError $ "No such var in env: " ++ name
   Just t -> return $ snd t

typecheck :: AST.Statement -> Typecheck ()
typecheck (AST.Block []) = _
