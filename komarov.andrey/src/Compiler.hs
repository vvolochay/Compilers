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

encode :: Type -> String
encode TInt = "word"
encode v = error $ "global " ++ show v ++ " variables are not supported yet!"

data FType = FType Type [Type]
           deriving (Show, Eq)

type Label = String

data Symbol
  = GlobalVariable { varType :: Type,
                     dataLabel :: Label,
                     textLabel :: Label}
  | LocalVariable { varType :: Type, varOffset :: Int }
  | ForwardDecl { funType :: FType }
  | FunctionDecl { funType :: FType, label :: Label }
  | Type Type
  deriving (Show)

newtype SymbolTable =
  SymbolTable { unSymbolTable :: M.Map Id Symbol}

data Env = Env {
  symbols :: SymbolTable,
  labels :: S.Set Label,
  offset :: Int,
  epilogue :: Maybe Label}

emptyEnv :: Env
emptyEnv = Env (SymbolTable M.empty) S.empty 0 Nothing

stdlib :: Env
stdlib = emptyEnv

setEpilogue :: Label -> Compiler ()
setEpilogue ep = do
  env@Env { epilogue = epilogue } <- get
  put $ env { epilogue = Just ep }

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
  Just (GlobalVariable { varType = t }) -> return t
  Just (LocalVariable { varType = t }) -> return t
  Just s -> throwError $ VariableExpected s

getType :: Id -> Compiler Type
getType name = symbol name >>= \case
  Nothing -> throwError $ SymbolNotDefined name
  Just (Type t) -> return t
  Just s -> throwError $ TypeExpected s

as :: Segment -> Assembly -> Compiler ()
as seg asm = tell $ Output [(seg, asm)]

updateGlobalVar :: Id -> Type -> Compiler ()
updateGlobalVar name t = symbol name >>= \case
  Nothing -> do
             dLabel <- fresh name
             tLabel <- fresh name
             setSymbol name $ GlobalVariable t dLabel tLabel
             as Data $ dLabel ++ ": .word 0"
             as Text $ tLabel ++ ": .word " ++ dLabel
  Just s' -> throwError $ AlreadyBound name s' $ GlobalVariable t "" ""

updateLocalVar :: Id -> Type -> Compiler ()
updateLocalVar name t = symbol name >>= \case
  Nothing -> do
    off <- gets offset
    let sz = size t
    modify $ \(env@Env { offset = o }) -> env { offset = o + sz }
    setSymbol name $ LocalVariable t (off + sz)
  Just s' -> throwError $ AlreadyBound name s' $ LocalVariable t 0
  


updateForwardDecl :: Id -> FType -> Compiler ()
updateForwardDecl name ty = symbol name >>= \case
  Nothing -> setSymbol name $ ForwardDecl ty
  Just (ForwardDecl ty') -> when (ty /= ty') $ throwError $ ForwardDeclTypeMismatch ty ty'
  Just (FunctionDecl { funType = ty' }) -> when (ty /= ty') $ throwError $ ForwardDeclTypeMismatch ty ty'
  Just s -> throwError $ AlreadyBound name s (ForwardDecl ty)

updateFun :: Id -> FType -> Compiler Label
updateFun name ty = symbol name >>= \case
  Nothing -> do
             lab <- fresh name
             setSymbol name $ FunctionDecl ty lab
             return lab
  Just (ForwardDecl ty') -> do
    when (ty /= ty') $ throwError $ ForwardDeclTypeMismatch ty ty'
    lab <- fresh name
    setSymbol name $ FunctionDecl ty lab
    return lab
  Just s -> throwError $ AlreadyBound name s (FunctionDecl ty "")

addLabel :: String -> Compiler ()
addLabel lab = do
  env@Env { labels = labels } <- get
  when (lab `S.member` labels) $ throwError $ LabelAlreadyDeclared lab
  put $ env { labels = S.insert lab labels }

fresh :: String -> Compiler String
fresh hint = do
  l <- gets labels
  let res = head $ [x | suf <- "":(map (('_':) . show) [1..]),
                      let x = hint ++ suf, not (x `S.member` l)]
  addLabel res
  return res

newtype Output = Output { unOutput :: [(Segment, Assembly)] }
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
  | TypeMismatch Type Type
  | LabelAlreadyDeclared String
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
    fname <- updateFun name (FType tret targs)
    ep <- fresh $ name ++ "_ep"
    as Text ""
    as Text $ "@ function " ++ show name
    forM args $ \(t, n) ->
      as Text $ "@   " ++ show n ++ " : " ++ show n
    as Text $ fname ++ ":"
    as Text $ "push {fp, lr}"
    as Text $ "mov fp, sp"
    -- TODO Correct stack frame size
    as Text $ "sub sp, sp, #16"
    setEpilogue ep
    mapM compile body
    as Text $ ep ++ ":"
    as Text $ "mov sp, fp"
    as Text $ "pop {fp, lr}"
    as Text $ "mov pc, lr"
    as Text $ "@ end of " ++ show name

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
  compile (AST.SAssignment name expr) = symbol name >>= \case
    Nothing -> throwError $ SymbolNotDefined name
    Just (LocalVariable tp off) -> do
      as Text $ "@ " ++ show name ++ " := " ++ show expr
      rhs <- compile expr
      when (tp /= rhs) $ throwError $ TypeMismatch tp rhs
      as Text $ "pop {r0}"
      as Text $ "@ storing to local " ++ name
      as Text $ "str r0, [sp, #-" ++ show off ++ "]"
      return Nothing
    Just (GlobalVariable tp dLabel tLabel) -> do
      as Text $ "@ " ++ show name ++ " := " ++ show expr
      rhs <- compile expr
      when (tp /= rhs) $ throwError $ TypeMismatch tp rhs
      as Text $ "ldr r1, " ++ tLabel
      as Text $ "pop {r0}"
      as Text $ "str r0, [r1]"
      return Nothing
    Just s -> throwError $ VariableExpected s
  compile (AST.SRawExpr expr) = do
    compile expr
    as Text $ "pop {r0} @ unused"
    return Nothing
  compile (AST.SIfThenElse cond thn els) = do
    undefined -- TODO too hard, skipping
  compile (AST.SWhile cond body) = do
    undefined -- TODO too hard, skipping
  compile (AST.SReturn expr) = do
    tp <- compile expr
    as Text $ "pop {r0}"
    Just ep <- gets epilogue
    as Text $ "b " ++ ep
    return $ Just tp

instance Compilable AST.Expression Type where
  compile _ = _
