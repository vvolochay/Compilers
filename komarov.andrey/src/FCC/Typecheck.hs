{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
module FCC.Typecheck (

  ) where

import FCC.Type
import FCC.TypecheckError
import FCC.Expr

import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as M

data Context a = Context {
  bindings :: M.Map a Type,
  expectedRetType :: Type
  }

lookupType :: Ord a => a -> Context a -> Maybe Type
lookupType v ctx = M.lookup v (bindings ctx)

newtype Typecheck a = Typecheck {
  runTypecheck :: ReaderT (Context String) (Except TypecheckError) a
  } deriving (Functor, Applicative, Monad,
              MonadError TypecheckError, MonadReader (Context String))

class Typecheckable (f :: * -> *) where
  typecheck :: f a -> Typecheck (f a, Type)

instance Typecheckable Expr where
  typecheck (Var v) = do
    ctx <- asks $ lookupType v
    case ctx of
     Nothing -> throwError $ UnboundVariable v
     Just t -> return (Var v, t)
  typecheck (Lit i) = return (Lit i, TInt)
  typecheck (LitBool b) = return (LitBool b, TBool)
  typecheck (Lam t s) = _
  typecheck Empty = return (Empty, TVoid)
  typecheck (Seq e1 e2) = do
    (e1', _) <- typecheck e1
    (e2', _) <- typecheck e2
    return $ (Seq e1' e2', TVoid)
  typecheck (Call f@(Var _) args) = do
    (f', tf) <- typecheck f
    args' <- mapM typecheck args
    let targs = fmap snd args'
    (tfargs, tfret) <- case tf of
      TFun ta t -> return (ta, t)
      t -> throwError $ NotAFunction t f
    when (targs /= tfargs) $ throwError $ ArgumentsTypesDiffer targs tfargs f
    return (f', tfret)
  typecheck (Call f args) = throwError $ NotCallable f
  typecheck (Eq e1 e2) = do
    (e1', te1) <- typecheck e1
    (e2', te2) <- typecheck e2
    when (te1 /= te2) $ throwError $ EqTypesDiffer te1 te2 e1 e2
    let select fname = return (Call (Var fname) [e1', e2'], TBool)
    case te1 of
     TInt -> select "_builtin_eq_int"
     TBool -> select "_builtin_eq_bool"
     TArray _ -> select "_builtin_eq_ptr"
     t -> throwError $ UnsupportedTypeForEq te1 e1 e2
  typecheck (While cond body) = do
    (cond', tcond) <- typecheck cond
    (body', tbody) <- typecheck body
    when (tcond /= TBool) $ throwError $ WhileConditionIsNotBool tcond cond
    return (While cond' body', TVoid)
  typecheck (If cond thn els) = do
    (cond', tcond) <- typecheck cond
    (thn', tthn) <- typecheck thn
    (els', tels) <- typecheck els
    when (tcond /= TBool) $ throwError $ IfConditionIsNotBool tcond cond
    return (If cond' thn' els', TVoid)
  typecheck (Assign v@(Var _) val) = do
    (v', tv) <- typecheck v
    (val', tval) <- typecheck val
    when (tv /= tval) $ throwError $ AssignTypeMismatch tv tval v val
    return (Assign v' val', TVoid)
  typecheck (Assign ai@(Array _ _) val) = do
    (ai', tai) <- typecheck ai
    (val', tval) <- typecheck val
    when (tai /= tval) $ throwError $ AssignTypeMismatch tai tval ai val
    return (Assign ai' val', TVoid)
  typecheck (Assign dst val) = throwError $ NotAssignable dst
  typecheck (Array a i) = do
    (a', ta) <- typecheck a
    (i', ti) <- typecheck i
    ta' <- case ta of
      TArray x -> return x
      t -> throwError $ NotAnArray ta a
    when (ti /= TInt) $ throwError $ IndexIsNotInt ti i
    return $ (Array a' i', ta')
  typecheck (Return e) = do
    (e', te) <- typecheck e
    tret <- asks expectedRetType
    when (te /= tret) $ throwError $ WrongReturnType te tret e
    return (Return e', TVoid)
