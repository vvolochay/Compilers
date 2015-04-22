{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FCC.PrettyPrinter (
  ppExpr
  ) where

import Control.Monad.RWS
import Data.List (intercalate)

import FCC.AST

type Offset = Int
type Output = [String]
newtype Config = Config { offsetStep :: Int }

defaultConfig = Config 4

newtype PrettyPrinter a = PrettyPrinter {
  runPrettyPrinter :: RWS Config Output Offset a
  } deriving (
  Functor, Applicative, Monad, MonadState Offset, MonadWriter Output, MonadReader Config
  )

ppline :: String -> PrettyPrinter ()
ppline s = do
  off <- get
  tell [(replicate off ' ') ++ s]

scoped :: PrettyPrinter a -> PrettyPrinter a
scoped p = do
  add <- asks offsetStep
  modify (+add)
  res <- p
  modify (`subtract` add)
  return res

class PrettyPrintable t where
  pprint :: t -> PrettyPrinter ()

ppExpr (EVar var) = var
ppExpr (ELitInt i) = show i
ppExpr (ELitBool True) = "true"
ppExpr (ELitBool False) = "false"
ppExpr (EArith op e1 e2) = "(" ++ ppExpr (value e1) ++ ") " ++ show op ++ " (" ++ ppExpr (value e2) ++ ")"
ppExpr (EBool op e1 e2) = "(" ++ ppExpr (value e1) ++ ") " ++ show op ++ " (" ++ ppExpr (value e2) ++ ")"
ppExpr (EArithCmp op e1 e2) = "(" ++ ppExpr (value e1) ++ ") " ++ show op ++ " (" ++ ppExpr (value e2) ++ ")"
ppExpr (EEqual op e1 e2) = "(" ++ ppExpr (value e1) ++ ") " ++ show op ++ " (" ++ ppExpr (value e2) ++ ")"
ppExpr (ECall f args) = f ++ "(" ++ (intercalate ", " $ map (ppExpr . value) args) ++ ")"
ppExpr (EAssign e1 e2) = ppExpr (value e1) ++ " = " ++ ppExpr (value e2)
ppExpr (EDeref e) = "*" ++ ppExpr (value e)
ppExpr (EAddr e) = "&" ++ ppExpr (value e)
ppExpr (EArray a i) = ppExpr (value a) ++ "[" ++ ppExpr (value i) ++ "]"
ppExpr (ECast t e) = "(" ++ show t ++ ")" ++ ppExpr (value e)


