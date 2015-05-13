module FCC.Type (
  Type(..),
  toPrimitiveType,
  ) where

import Data.List (intercalate)

data Type
  = TInt
  | TBool
  | TArray Type
  | TTuple [Type]
  | TFun Type Type
  deriving (Eq, Ord, Show, Read)

toPrimitiveType :: String -> Type
toPrimitiveType "int" = TInt
toPrimitiveType "bool" = TBool
toPrimitiveType t = error $ "BEDA " ++ t

{-
instance Show Type where
  show TInt = "int"
  show TBool = "bool"
  show (TArray t) = show t ++ "*"
  show (TTuple ts) = "(" ++ intercalate ", " (map show ts) ++ ")"
  show (TFun from to) = "{" ++ show from ++ " -> " ++ show to ++ "}"
-}
