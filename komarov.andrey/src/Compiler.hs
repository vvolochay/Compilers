{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler (
  runCompiler,
  output
  ) where

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative

import Data.List
import Data.Function
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

type Label = String

data Symbol
  = GlobalVariable { varType :: Type,
                     textLabel :: Label}
  | LocalVariable { varType :: Type, varOffset :: Int }
  | ForwardDecl { funType :: FType, label :: Label }
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

put' :: Env -> Compiler ()
put' env' = do
  env <- get
  put $ env' { labels = labels env }

emptyEnv :: Env
emptyEnv = Env (SymbolTable M.empty) S.empty 0 Nothing

stdTable :: SymbolTable
stdTable = SymbolTable $ M.fromList $ [
  ("int", Type TInt),
  ("bool", Type TBool),
  ("void", Type TVoid)]

stdlib :: Env
stdlib = emptyEnv { symbols = stdTable }

setOffset :: Int -> Compiler ()
setOffset off = modify $ \env -> env { offset = off }

setSymbols :: SymbolTable -> Compiler ()
setSymbols s = modify $ \env -> env { symbols = s }

setEpilogue :: Label -> Compiler ()
setEpilogue ep = do
  env <- get
  put $ env { epilogue = Just ep }

symbol :: Id -> Compiler (Maybe Symbol)
symbol name = do
  sym <- gets (unSymbolTable . symbols)
  return $ M.lookup name sym

setSymbol :: Id -> Symbol -> Compiler ()
setSymbol name s = do
  env@(Env { symbols = SymbolTable syms }) <- get
  put $ env { symbols = SymbolTable $ M.insert name s syms }

getFun :: Id -> Compiler (FType, Label)
getFun name = symbol name >>= \case
  Nothing -> throwError $ SymbolNotDefined name
  Just (ForwardDecl { funType = t, label = l }) -> return (t, l)
  Just (FunctionDecl { funType = t, label = l }) -> return (t, l)
  Just s -> throwError $ FunctionExpected s

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
             setSymbol name $ GlobalVariable t tLabel
             as Data $ dLabel ++ ": .word 0"
             as Text $ tLabel ++ ": .word " ++ dLabel
  Just s' -> throwError $ AlreadyBound name s' $ GlobalVariable t ""

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
  Nothing -> do
    lab <- fresh name
    when (lab /= name) $ throwError $ LabelAlreadyDeclared name
    setSymbol name $ ForwardDecl ty lab
  Just (ForwardDecl ty' _) -> when (ty /= ty') $ throwError $ ForwardDeclTypeMismatch ty ty'
  Just (FunctionDecl { funType = ty' }) -> when (ty /= ty') $ throwError $ ForwardDeclTypeMismatch ty ty'
  Just s -> throwError $ AlreadyBound name s (ForwardDecl ty "")

updateFun :: Id -> FType -> Compiler Label
updateFun name ty = symbol name >>= \case
  Nothing -> do
             lab <- fresh name
             setSymbol name $ FunctionDecl ty lab
             as Data $ ".global " ++ name
             return lab
  Just (ForwardDecl ty' lab) -> do
    when (ty /= ty') $ throwError $ ForwardDeclTypeMismatch ty ty'
    setSymbol name $ FunctionDecl ty lab
    as Data $ ".global " ++ name
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

mergeTypes :: [Maybe Type] -> Compiler (Maybe Type)
mergeTypes types = case nub $ catMaybes types of
  [] -> return Nothing
  [t] -> return $ Just t
  ts -> throwError $ InconsistentReturnTypes ts

newtype Output = Output { unOutput :: [(Segment, Assembly)] }
               deriving (Show, Monoid)

output :: Output -> String
output (Output out) =
  intercalate "\n\n" $ map (uncurry sect) sections where
    sect :: Segment -> [String] -> String
    sect seg lines = "@@@@@@@@@@@@@@@\n." ++ show seg ++ "\n\n" ++ intercalate "\n" lines

    toSect :: [(a, b)] -> (a, [b])
    toSect pairs = let (a:_,b) = unzip pairs in (a, b)
  
    sections :: [(Segment, [String])]
    sections = map toSect $ groupBy ((==) `on` fst)
               $ sortBy (compare `on` fst) out

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
  | WrongArgsNumber [Type] [Type]
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
    getType ty >>= updateGlobalVar name
  compile (AST.ForwardDecl name ret args) = do
    tret <- getType ret
    targs <- mapM getType args
    updateForwardDecl name (FType tret targs)
  compile (AST.FuncDef name ret args body) = do
    tret <- getType ret
    targs <- mapM getType (map fst args)
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
    as Text $ "sub sp, sp, #128"
    setEpilogue ep
    oldSymbols <- gets symbols
    let argPairs = zip (map snd args) targs
    let stackArgs = drop 4 argPairs
    let registerArgs = take 4 argPairs
    setOffset $ -(4 * length stackArgs + 8)
    -- TODO incorrect. "push{fp,lr}" splits args to reg/stack groups
    mapM (uncurry updateLocalVar) $ reverse stackArgs
    setOffset 0
    mapM (uncurry updateLocalVar) registerArgs
    case length registerArgs of
     0 -> return ()
     1 -> as Text $ "str r0, [fp, #-4]"
     2 -> mapM_ (as Text) ["str r0, [fp, #-4]", "str r1, [fp, #-8]"]
     3 -> mapM_ (as Text) ["str r0, [fp, #-4]", "str r1, [fp, #-8]", "str r2, [fp, #-12]"]
     4 -> mapM_ (as Text) ["str r0, [fp, #-4]", "str r1, [fp, #-8]", "str r2, [fp, #-12]", "str r3, [fp, #-16]"]
     n -> error "IMPOSSIBLE"
    mapM compile body
    setOffset 0
    setSymbols oldSymbols
    as Text $ ep ++ ":"
    as Text $ "mov sp, fp"
    as Text $ "pop {fp, lr}"
    as Text $ "mov pc, lr"
    as Text $ "@ end of " ++ show name

instance Compilable AST.Statement (Maybe Type) where
  compile (AST.SBlock stmts) = do
    env <- get
    types <- mapM compile stmts
    put' env
    mergeTypes types
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
      as Text $ "str r0, [fp, #-" ++ show off ++ "]"
      return Nothing
    Just (GlobalVariable tp tLabel) -> do
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
    elseLabel <- fresh "else"
    endifLabel <- fresh "endif"
    as Text $ "@ if"
    t <- compile cond
    when (t /= TBool) $ throwError $ TypeMismatch t TBool
    as Text $ "pop {r0}"
    as Text $ "@ then"
    as Text $ "teq r0, #0"
    as Text $ "beq " ++ elseLabel
    thnType <- compile thn
    as Text $ "b " ++ endifLabel
    as Text $ elseLabel ++ ":"
    elsType <- compile els
    retType <- mergeTypes [thnType, elsType]
    as Text $ endifLabel ++ ":"
    return retType
  compile (AST.SWhile cond body) = do
    [whileLabel, endWhileLabel] <- mapM fresh ["while", "endwhile"]
    as Text $ whileLabel ++ ":"
    tcond <- compile cond
    when (tcond /= TBool) $ throwError $ TypeMismatch tcond TBool
    as Text $ "pop {r0}"
    as Text $ "teq r0, #0"
    as Text $ "beq " ++ endWhileLabel
    tret <- compile body
    as Text $ "b " ++ whileLabel
    as Text $ endWhileLabel ++ ":"
    return tret
  compile (AST.SReturn expr) = do
    tp <- compile expr
    as Text $ "pop {r0}"
    Just ep <- gets epilogue
    as Text $ "b " ++ ep
    return $ Just tp

instance Compilable AST.Expression Type where
  compile (AST.EVar v) = symbol v >>= \case
    Nothing -> throwError $ SymbolNotDefined v
    Just (LocalVariable tp off) -> do
      as Text $ "@ local " ++ show v
      as Text $ "ldr r0, [fp, #-" ++ show off ++ "]"
      as Text $ "push {r0}"
      return tp
    Just (GlobalVariable tp tLabel) -> do
      as Text $ "@ global " ++ show v
      as Text $ "ldr r0, " ++ tLabel
      as Text $ "ldr r0, [r0]"
      as Text $ "push {r0}"
      return tp
    Just s -> throwError $ VariableExpected s
  compile (AST.EBool b) = do
    as Text $ "ldr r0, =" ++ show (if b then 1 else 0)
    as Text $ "push {r0}"
    return TBool
  compile (AST.EInt i) = do
    as Text $ "ldr r0, =" ++ show i
    as Text $ "push {r0}"
    return TInt
  compile (AST.EAdd lhs rhs) = do
    tl <- compile lhs
    tr <- compile rhs
    when (tl /= TInt) $ throwError $ TypeMismatch tl TInt
    when (tr /= TInt) $ throwError $ TypeMismatch tr TInt
    as Text $ "pop {r0, r1}"
    as Text $ "add r0, r1, r0"
    as Text $ "push {r0}"
    return TInt
  compile (AST.ESub lhs rhs) = do
    tl <- compile lhs
    tr <- compile rhs
    when (tl /= TInt) $ throwError $ TypeMismatch tl TInt
    when (tr /= TInt) $ throwError $ TypeMismatch tr TInt
    as Text $ "pop {r0, r1}"
    as Text $ "sub r0, r1, r0"
    as Text $ "push {r0}"
    return TInt
  compile (AST.EMul lhs rhs) = do
    tl <- compile lhs
    tr <- compile rhs
    when (tl /= TInt) $ throwError $ TypeMismatch tl TInt
    when (tr /= TInt) $ throwError $ TypeMismatch tr TInt
    as Text $ "pop {r0, r1}"
    as Text $ "mul r0, r1, r0"
    as Text $ "push {r0}"
    return TInt
  compile (AST.ELess lhs rhs) = do
    tl <- compile lhs
    tr <- compile rhs
    when (tl /= TInt) $ throwError $ TypeMismatch tl TInt
    when (tr /= TInt) $ throwError $ TypeMismatch tr TInt
    as Text $ "pop {r0, r1}"
    as Text $ "cmp r1, r0"
    as Text $ "movlt r0, #1"
    as Text $ "movge r0, #0"
    as Text $ "push {r0}"
    return TBool
  compile (AST.EGreater lhs rhs) = do
    tl <- compile lhs
    tr <- compile rhs
    when (tl /= TInt) $ throwError $ TypeMismatch tl TInt
    when (tr /= TInt) $ throwError $ TypeMismatch tr TInt
    as Text $ "pop {r0, r1}"
    as Text $ "cmp r1, r0"
    as Text $ "movgt r0, #1"
    as Text $ "movle r0, #0"
    as Text $ "push {r0}"
    return TBool
  compile (AST.ELessEq lhs rhs) = do
    tl <- compile lhs
    tr <- compile rhs
    when (tl /= TInt) $ throwError $ TypeMismatch tl TInt
    when (tr /= TInt) $ throwError $ TypeMismatch tr TInt
    as Text $ "pop {r0, r1}"
    as Text $ "cmp r1, r0"
    as Text $ "movle r0, #1"
    as Text $ "movgt r0, #0"
    as Text $ "push {r0}"
    return TBool
  compile (AST.EGreaterEq lhs rhs) = do
    tl <- compile lhs
    tr <- compile rhs
    when (tl /= TInt) $ throwError $ TypeMismatch tl TInt
    when (tr /= TInt) $ throwError $ TypeMismatch tr TInt
    as Text $ "pop {r0, r1}"
    as Text $ "cmp r1, r0"
    as Text $ "movge r0, #1"
    as Text $ "movlt r0, #0"
    as Text $ "push {r0}"
    return TBool
  compile (AST.EEqual lhs rhs) = do
    tl <- compile lhs
    tr <- compile rhs
    when (tl /= tr) $ throwError $ TypeMismatch tl tr
    as Text $ "pop {r0, r1}"
    as Text $ "teq r0, r1"
    as Text $ "moveq r0, #1"
    as Text $ "movne r0, #0"
    as Text $ "push {r0}"
    return TBool
  compile (AST.ENotEqual lhs rhs) = do
    tl <- compile lhs
    tr <- compile rhs
    when (tl /= tr) $ throwError $ TypeMismatch tl tr
    as Text $ "pop {r0, r1}"
    as Text $ "teq r0, r1"
    as Text $ "movne r0, #1"
    as Text $ "moveq r0, #0"
    as Text $ "push {r0}"
    return TBool
  compile (AST.EAnd lhs rhs) = do
    tl <- compile lhs
    tr <- compile rhs
    when (tl /= TBool) $ throwError $ TypeMismatch tl TBool
    when (tr /= TBool) $ throwError $ TypeMismatch tr TBool
    as Text $ "pop {r0, r1}"
    as Text $ "and r0, r1, r0"
    as Text $ "push {r0}"
    return TBool
  compile (AST.EOr lhs rhs) = do
    tl <- compile lhs
    tr <- compile rhs
    when (tl /= TBool) $ throwError $ TypeMismatch tl TBool
    when (tr /= TBool) $ throwError $ TypeMismatch tr TBool
    as Text $ "pop {r0, r1}"
    as Text $ "orr r0, r1, r0"
    as Text $ "push {r0}"
    return TBool
  compile (AST.ECall name args) = do
    targs <- reverse <$> (mapM compile $ reverse args)
    (FType ret targs', label) <- getFun name
    when (length targs /= length targs') $
      throwError $ WrongArgsNumber targs targs'
    forM (zip targs targs') $ \(t, t') ->
      when (t /= t') $ throwError $ TypeMismatch t t'
    case (length args) of
     0 -> as Text $ "@ no args"
     1 -> as Text $ "pop {r0}"
     2 -> mapM_ (as Text) ["pop {r0}", "pop {r1}"]
     3 -> mapM_ (as Text) ["pop {r0}", "pop {r1}", "pop {r2}"]
     _ -> mapM_ (as Text) ["pop {r0}", "pop {r1}", "pop {r2}", "pop {r3}"]
    as Text $ "bl " ++ label
    as Text $ "push {r0}"
    return ret
