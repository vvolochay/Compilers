{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler (

  ) where

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative

import Data.List (nub)
import Data.Maybe (catMaybes)

import qualified Data.Map as M
import qualified Data.Set as S

import ARM
import AST (Id)
import qualified AST

data Type = TBool
          | TInt
          | TString
          | TVoid
          deriving (Show, Eq)

size :: Num a => Type -> a
size TBool = 4
size TInt = 4
size TString = error "lol not implemented yet"
size TVoid = error "lol void is not instantiable"


data FType = FType Type [Type]
           deriving (Show, Eq)

data Symbol
  = GlobalVariable { varType :: Type }
  | LocalVariable { varType :: Type, varOffset :: Int }
  | ForwardDecl { funType :: FType }
  | FunctionDecl { funType :: FType }
  | Type Type
  deriving (Show)

newtype SymbolTable =
  SymbolTable { unSymbolTable :: M.Map Id Symbol}

data Env = Env {
  symbols :: SymbolTable,
  labels :: S.Set Id,
  offset :: Int}

emptyEnv :: Env
emptyEnv = Env (SymbolTable M.empty) S.empty 0

stdlib :: Env
stdlib = emptyEnv

symbol :: Id -> Compiler (Maybe Symbol)
symbol name = do
  sym <- gets (unSymbolTable . symbols)
  return $ M.lookup name sym

setSymbol :: Id -> Symbol -> Compiler ()
setSymbol name s = do
  env@(Env { symbols = SymbolTable syms }) <- get
  put $ env { symbols = SymbolTable $ M.insert name s syms }

getVarType :: Id -> Compiler Type
getVarType name = symbol name >>= \case
  Nothing -> throwError $ SymbolNotDefined name
  Just (GlobalVariable t) -> return t
  Just (LocalVariable { varType = t } ) -> return t
  Just s -> throwError $ VariableExpected s

getType :: Id -> Compiler Type
getType name = symbol name >>= \case
  Nothing -> throwError $ SymbolNotDefined name
  Just (Type t) -> return t
  Just s -> throwError $ TypeExpected s

updateGlobalVar :: Id -> Type -> Compiler ()
updateGlobalVar name t = symbol name >>= \case
  Nothing -> setSymbol name $ GlobalVariable t
  Just s' -> throwError $ AlreadyBound name s' $ GlobalVariable t

updateLocalVar :: Id -> Type -> Compiler ()
updateLocalVar name t = symbol name >>= \case
  Nothing -> do
    off <- gets offset
    let sz = size t
    modify $ \(env@Env { offset = o }) -> env { offset = o + sz }
    setSymbol name $ LocalVariable t off
  Just s' -> throwError $ AlreadyBound name s' $ LocalVariable t 0
  


updateForwardDecl :: Id -> FType -> Compiler ()
updateForwardDecl name ty = symbol name >>= \case
  Nothing -> setSymbol name $ ForwardDecl ty
  Just (ForwardDecl ty') -> when (ty /= ty') $ throwError $ ForwardDeclTypeMismatch ty ty'
  Just (FunctionDecl ty') -> when (ty /= ty') $ throwError $ ForwardDeclTypeMismatch ty ty'
  Just s -> throwError $ AlreadyBound name s (ForwardDecl ty)

updateFun :: Id -> FType -> Compiler ()
updateFun name ty = symbol name >>= \case
  Nothing -> setSymbol name $ FunctionDecl ty
  Just (ForwardDecl ty') -> do
    when (ty /= ty') $ throwError $ ForwardDeclTypeMismatch ty ty'
    setSymbol name $ FunctionDecl ty
  Just s -> throwError $ AlreadyBound name s (FunctionDecl ty)

newtype Output = Output { unOutput :: [Assembly] }
               deriving (Show, Monoid)

data CompileError
  = CompileError
  | SymbolNotDefined Id
  | AlreadyBound Id Symbol Symbol
  | VariableExpected Symbol
  | TypeExpected Symbol
  | FunctionExpected Symbol
  | ForwardDeclTypeMismatch FType FType
  | InconsistentReturnTypes [Type]
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
    getType ty >>= updateLocalVar name
  compile (AST.ForwardDecl name ret args) = do
    tret <- getVarType ret
    targs <- mapM getVarType args
    updateForwardDecl name (FType tret targs)
  compile (AST.FuncDef name ret args body) = do
    tret <- getVarType ret
    targs <- mapM getVarType (map fst args)
    updateFun name (FType tret targs)

instance Compilable AST.Statement (Maybe Type) where
  compile (AST.SBlock stmts) = do
    env <- get
    types <- mapM compile stmts
    put env
    case nub $ catMaybes types of
     [] -> return Nothing
     [t] -> return $ Just t
     ts -> throwError $ InconsistentReturnTypes ts
  compile (AST.SVarDecl tp name) =
    getType tp >>= updateLocalVar name >> return Nothing
  compile (AST.SAssignment name expr) = do
    -- TODO generate some assembly here
    _
    return Nothing
