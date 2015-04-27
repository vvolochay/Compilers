{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
module FCC.AST (
  Id,
  Tagged,
  Type(..),
  Program(..),
  TopLevel(..),
  Statement(..),
  Expression(..),
  ArithBinOp(..),
  ArithCmpOp(..),
  BoolBinOp(..),
  EqOp(..),
  notag, with, value,
  toPrimitiveType
  ) where

import Data.List (intercalate)

type Id = String
type Tagged f a = (f a, a)

tag :: Tagged f a -> a
tag = snd

value :: Tagged f a -> f a
value = fst

notag :: f () -> Tagged f ()
notag x = (x, ())

with :: f a -> a -> Tagged f a
with = (,)

data Program a = Program [TopLevel a]
          deriving (Eq, Functor)

data Type
  = TInt
  | TBool
  | TVoid
  | TPointer Type
  deriving (Eq, Ord)

toPrimitiveType :: String -> Type
toPrimitiveType "int" = TInt
toPrimitiveType "bool" = TBool
toPrimitiveType "void" = TVoid
toPrimitiveType t = error $ "INTERNAL COMPILER ERROR: type <" ++ t ++ "> not recognized"

data TopLevel a
  = VarDecl Type Id
  | ForwardDecl { name :: Id,
                  ret :: Type,
                  argsTypes :: [Type] }
  | FuncDef { name :: Id,
              ret :: Type,
              args :: [(Type, Id)],
              body :: Statement a}
  deriving (Eq, Functor)

data Statement a = SBlock [Statement a]
                 | SVarDecl Type Id
                 | SAssignment Id (Tagged Expression a)
                 | SRawExpr (Tagged Expression a)
                 | SIfThenElse (Tagged Expression a) (Statement a) (Statement a)
                 | SWhile (Tagged Expression a) (Statement a)
                 | SReturn (Tagged Expression a)
                 deriving (Eq, Functor)

data ArithBinOp = AddOp | SubOp | MulOp
                deriving (Eq, Ord)
data BoolBinOp = OrOp | AndOp | XorOp
               deriving (Eq, Ord)
data ArithCmpOp = LessOp | LessEqOp | GreaterOp | GreaterEqOp
                deriving (Eq, Ord)
data EqOp = EqOp | NeqOp
          deriving (Eq, Ord)

instance Show ArithBinOp where
  show AddOp = "+"
  show SubOp = "-"
  show MulOp = "*"

instance Show BoolBinOp where
  show OrOp = "||"
  show AndOp = "&&"
  show XorOp = "^"

instance Show ArithCmpOp where
  show LessOp = "<"
  show LessEqOp = "<="
  show GreaterOp = ">"
  show GreaterEqOp = ">="

instance Show EqOp where
  show EqOp = "=="
  show NeqOp = "!="

data Expression a = EVar Id
                  | ELitInt Int
                  | ELitBool Bool
                  | EArith ArithBinOp (Tagged Expression a) (Tagged Expression a)
                  | EBool BoolBinOp (Tagged Expression a) (Tagged Expression a)
                  | EArithCmp ArithCmpOp (Tagged Expression a) (Tagged Expression a)
                  | EEqual EqOp (Tagged Expression a) (Tagged Expression a)
                  | ECall Id [(Tagged Expression a)]
                  | EAssign (Tagged Expression a) (Tagged Expression a)
                  | EDeref (Tagged Expression a)
                  | EAddr (Tagged Expression a)
                  | EArray (Tagged Expression a) (Tagged Expression a)
                  | ECast Type (Tagged Expression a)
                  deriving (Eq, Ord, Functor)

ppExpr :: Expression a -> String
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

instance Show (Expression a) where
  show = ppExpr

ppStmt :: Int -> Statement a -> String
ppStmt off (SBlock stmts) = "{\n" ++ (intercalate "\n" $ map (ppStmt (off + 4)) stmts) ++ "\n}"
ppStmt off (SVarDecl tp id) = replicate off ' ' ++ show tp ++ " " ++ id ++ ";"
ppStmt off (SRawExpr e) = replicate off ' ' ++ show (value e) ++ ";"
ppStmt off (SIfThenElse cond thn els) = if' ++ "\n" ++ then' ++ "\nelse" ++ else' where
  if' = replicate off ' ' ++ "if (" ++ (show (value cond)) ++ ")"
  then' = ppStmt (off + 4) thn
  else' = ppStmt (off + 4) els
ppStmt off (SWhile cond body) = while' ++ "\n" ++ body' where
  while' = replicate off ' ' ++ "while (" ++ show (value cond) ++ ")"
  body' = ppStmt (off + 4) body
ppStmt off (SReturn ret) = replicate off ' ' ++ show (value ret) ++ ";"

instance Show (Statement a) where
  show = ppStmt 0

ppTopLevel :: TopLevel a -> String
ppTopLevel (VarDecl tp id) = show tp ++ " " ++ id ++ ";"
ppTopLevel ForwardDecl{..} = show ret ++ " " ++ name ++ "(" ++ (intercalate ", " args') ++ ");" where 
  args' = zipWith (\t i -> show t ++ " arg" ++ show i) argsTypes [1..]
ppTopLevel FuncDef{..} = show ret ++ " " ++ name ++ "(" ++ (intercalate ", " (map showPair args)) ++ ")" ++ show body where
  showPair (a, b) = show a ++ " " ++ b

instance Show (TopLevel a) where
  show = ppTopLevel

instance Show (Program a) where
  show (Program t) = intercalate "\n\n" $ map show t

instance Show Type where
  show TInt = "int"
  show TBool = "bool"
  show TVoid = "void"
  show (TPointer t) = show t ++ "*"
