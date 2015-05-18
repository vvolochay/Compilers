{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module FCC.Codegen (

  ) where

import FCC.Expr
import FCC.Program

import Bound

import Control.Monad.State

impossible = error "Internal compiler error. Please submit a bug-report."

data Binding = Local Int | Global String | Arg Int
                           deriving (Eq, Ord, Show, Read)

data CodegenState = CodegenState { offset :: Int, counter :: Int, maxOffset :: Int }

modifyOffset :: MonadState CodegenState m => (Int -> Int) -> m ()
modifyOffset f = modify $ \s@(CodegenState{maxOffset = m, offset = o})
                          -> s{maxOffset = m `max` o, offset = f o}

newtype Codegen a = Codegen {
  runCodegen :: State CodegenState a
  } deriving (Functor, Applicative, Monad, MonadState CodegenState)

fresh :: String -> Codegen String
fresh pref = do
  x <- gets counter
  modify (\s -> s{counter = x + 1})
  return $ pref ++ show x

freshLabel :: Codegen String
freshLabel = fresh "_label_"

freshVar :: Codegen String
freshVar = fresh "_var_"

codegen :: Program a -> [String] 
codegen = _

compileE :: Expr Binding -> Codegen [String]
compileE (Var (Local off)) = return ["ldr r0, [fp, #-" ++ show (off * 4) ++"]", "push r0"]
compileE (Var (Global name)) = return ["ldr r0, " ++ show name, "push r0"]
compileE (Var (Arg arg)) = return ["ldr r0, [fp, #" ++ show (arg * 4 + 8) ++ "]", "push r0"]
compileE (Lit i) = return ["push =" ++ show i]
compileE (LitBool True) = return ["push #1\t\t@ true"]
compileE (LitBool False) = return ["push #0\t\t@ false"]
compileE (Lam t s) = do
  v <- freshVar
  off <- gets offset
  modifyOffset (+1)
  code <- compileE $ instantiate1 (Var (Local off)) s
  modifyOffset (-1)
  return code
compileE Empty = return []
compileE (Pop e) = do
  code <- compileE e
  return $ code ++ ["pop r0"]
compileE (Seq e1 e2) = (++) <$> compileE e1 <*> compileE e2
compileE (Call (Var (Global fname)) args) = do
  compiledArgs <- reverse <$> concat <$> mapM compileE args
  return $ compiledArgs ++ ["bl " ++ fname]
compileE (Call _ _) = impossible
compileE (Eq _ _) = impossible
compileE (While cond body) = do
  begin <- freshLabel
  end <- freshLabel
  cond' <- compileE cond
  body' <- compileE body
  return $ [begin ++ ": @ while"] ++ cond' ++ ["pop r0", "tst r0, r0", "bz " ++ end] ++ body' ++ [end ++ ": @ endwhile"]
compileE (If cond thn els) = do
  elseLabel <- freshLabel
  endIfLabel <- freshLabel
  cond' <- compileE cond
  thn' <- compileE thn
  els' <- compileE els
  return $ cond' ++ ["pop r0", "tst r0, r0", "bz " ++ elseLabel]
    ++ thn' ++ ["b " ++ endIfLabel, elseLabel ++ ": @ else:"]
    ++ els' ++ [endIfLabel ++ ": @ endif"]
compileE (Assign (Var (Local off)) src) = do
  code <- compileE src
  return $ code ++ ["pop r0", "str r0, [fp, #-" ++ show (off * 4) ++ "]", "push r0"]
compileE (Assign (Var (Global name)) src) = do
  code <- compileE src
  return $ code ++ ["pop r0", "ldr r1, " ++ show name, "str r0, [r1]", "push r0"]
compileE (Assign (Var (Arg arg)) src) = do
  code <- compileE src
  return $ code ++ ["pop r0", "str r0, [fp, #" ++ show (arg * 4 + 8) ++ "]", "push r0"]
compileE (Assign (Array a i) src) = do
  codea <- compileE a
  codei <- compileE i
  code <- compileE src
  return $ codea ++ codei ++ code ++ ["pop r0\t\t@ b", "pop r1\t\t@ i", "pop r2\t\t@ a", "str r0, [r2, r1, LSL #2]", "push r0"]
compileE (Assign _ _) = impossible
compileE (Array a i) = do
  codea <- compileE a
  codei <- compileE i
  return $ codea ++ codei ++ ["pop r1", "pop r0", "ldr r0, [r0, r1, LSL #2]", "push r0"]
compileE (Return e) = do
  code <- compileE e
  return $ code ++ ["pop r0", "mov sp, fp", "pop {fp, lr}", "mov pc, lr"]
