{-# LANGUAGE GADTs, KindSignatures, DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators, RankNTypes, ImpredicativeTypes #-}
module TypedAST (

  ) where

import qualified Data.Map as M

type Id = String

data Type = TBool
          | TInt
          | TString
          | TVoid

data LiftType :: Type -> * where
  LiftBool :: LiftType TBool
  LiftInt :: LiftType TInt
  LiftString :: LiftType TString
  LiftVoid :: LiftType TVoid

type Env = Id -> Type
type Scope = [(Id, Type)]

data TStatement :: Type -> * where
  TBlock :: TStatement t -> [TStatement t] -> TStatement t
  TEmpty :: TStatement t
  TVariableDeclaration :: Type -> [Id] -> TStatement t
  TAssignment :: Id -> TExpr t' -> TStatement t
  TRawExpression :: TExpr t' -> TStatement t
  TIfThenElse :: TExpr TBool -> TStatement t -> TStatement t ->
                 TStatement t
  TWhile :: TExpr TBool -> TStatement t -> TStatement t
  TReturn :: TExpr t -> TStatement t
  

data TFunction :: [Type] -> Type -> * where
  TFunction :: Types argTypes -> Scope -> TStatement ret ->
               TFunction argTypes ret

data TList (f :: k -> *) :: [k] -> * where
  Nil :: TList f '[]
  Cons :: f t -> TList f tx -> TList f (t ': tx)

type TExprs = TList TExpr
type Types = TList LiftType

data TExpr :: Type -> * where
  TEInt :: Int -> TExpr TInt
  TEBool :: Bool -> TExpr TBool
  TEAddInt :: TExpr TInt -> TExpr TInt -> TExpr TInt
  TESub :: TExpr TInt -> TExpr TInt -> TExpr TInt
  TEMul :: TExpr TInt -> TExpr TInt -> TExpr TInt
  TELess :: TExpr TInt -> TExpr TInt -> TExpr TBool
  TEGreater :: TExpr TInt -> TExpr TInt -> TExpr TBool
  TELessEq :: TExpr TInt -> TExpr TInt -> TExpr TBool
  TEGreaterEq :: TExpr TInt -> TExpr TInt -> TExpr TBool
  TEEqual :: TExpr t -> TExpr t -> TExpr TBool
  TENotEqual :: TExpr t -> TExpr t -> TExpr TBool
  TEAnd :: TExpr TBool -> TExpr TBool -> TExpr TBool
  TEOr :: TExpr TBool -> TExpr TBool -> TExpr TBool
  TECall :: TFunction args ret -> TExprs args -> TExpr res

