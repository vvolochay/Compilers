module AST (
  Id,
  Program(..),
  TopLevel(..),
  Statement(..),
  Expression(..)
  ) where

type Id = String

data Program a = Program [TopLevel a]
          deriving (Show)

data TopLevel a
  = VarDecl Id Id
  | ForwardDecl { name :: Id,
                  ret :: Id,
                  argsTypes :: [Id] }
  | FuncDef { name :: Id,
              ret :: Id,
              args :: [(Id, Id)],
              body :: [Statement a]}
  deriving (Show)

data Statement a = SBlock [Statement a]
                 | SVarDecl Id Id
                 | SAssignment Id (Tagged Expression a)
                 | SRawExpr (Tagged Expression a)
                 | SIfThenElse (Tagged Expression a) (Statement a) (Statement a)
                 | SWhile (Tagged Expression a) (Statement a)
                 | SReturn (Tagged Expression a)
                 deriving (Show)

type Tagged f a = (f a, a)

data ArithBinOp = AddOp | SubOp | MulOp
                deriving (Eq, Ord)
data BoolBinOp = OrOp | AndOp | XorOp
               deriving (Eq, Ord)
data ArithCmpOp = LessOp | LessEqOp | GreaterOp | GreaterEqOp
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

data Expression a = EVar Id
                  | ELitInt Int
                  | ELitBool Bool
                  | EArith ArithBinOp (Tagged Expression a) (Tagged Expression a)
                  | EBool BoolBinOp (Tagged Expression a) (Tagged Expression a)
                  | EArithCmpOp ArithCmpOp (Tagged Expression a) (Tagged Expression a)
                  | EEqual (Tagged Expression a) (Tagged Expression a)
                  | ENotEqual (Tagged Expression a) (Tagged Expression a)
                  | ECall Id [(Tagged Expression a)]
                  deriving (Show, Eq, Ord)

