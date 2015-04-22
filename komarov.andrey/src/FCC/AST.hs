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
  notag, with
  ) where

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
          deriving (Show)

data Type
  = TInt
  | TBool
  | TPointer Type
  deriving (Show, Eq, Ord)

data TopLevel a
  = VarDecl Type Id
  | ForwardDecl { name :: Id,
                  ret :: Type,
                  argsTypes :: [Type] }
  | FuncDef { name :: Id,
              ret :: Type,
              args :: [(Type, Id)],
              body :: Statement a}
  deriving (Show)

data Statement a = SBlock [Statement a]
                 | SVarDecl Type Id
                 | SAssignment Id (Tagged Expression a)
                 | SRawExpr (Tagged Expression a)
                 | SIfThenElse (Tagged Expression a) (Statement a) (Statement a)
                 | SWhile (Tagged Expression a) (Statement a)
                 | SReturn (Tagged Expression a)
                 deriving (Show)

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
                  deriving (Show, Eq, Ord)
