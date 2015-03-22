{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Compiler (

  ) where

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative

import qualified Data.Map as M
import qualified Data.Set as S

import ARM
import AST

data Type = TBool
          | TInt
          | TString
          | TVoid
          deriving (Show, Eq)

data FType = FType Type [Type]

data Env = Env {
  forwardDecls :: M.Map Id FType,
  functions :: M.Map Id FType,
  variables :: M.Map Id Type,
  labels :: S.Set Id}

emptyEnv :: Env
emptyEnv = Env M.empty M.empty M.empty S.empty

stdlib :: Env
stdlib = emptyEnv
newtype Output = Output { unOutput :: [Assembly] }
               deriving (Show, Monoid)

data CompileError = CompileError
                    deriving (Show)

instance Error CompileError where
  noMsg = CompileError

newtype Compiler a = Compiler {
  unCompiler ::
     ErrorT CompileError (
       WriterT Output (
          State Env )) a }
                     deriving (
  Functor, Applicative, Monad, MonadError CompileError,
  MonadWriter Output, MonadState Env)


runCompiler :: AST.Program -> Either CompileError Output
runCompiler prog = fmap (const out) e where
  ((e, out), env) = runState (runWriterT $ runErrorT $ unCompiler $ compile prog) stdlib

class Compilable t ret | t -> ret where
  compile :: t -> Compiler ret

instance Compilable AST.Program () where
  compile (AST.Program xs) = mapM_ compile xs

instance Compilable AST.TopLevel () where
  compile (AST.VarDecl ty name) =
    parseType tp >>= add
