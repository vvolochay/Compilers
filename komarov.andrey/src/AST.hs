module AST (
  Id,
  Type(..),
  Program(..),
  TopLevel(..),
  Statement(..),
  Expression(..)
  ) where

type Id = String

data Program = Program [TopLevel]
          deriving (Show)

data Type
  = Simple Id
  | Pointer Type
  deriving (Show, Eq, Ord)

data TopLevel
  = VarDecl Type Id
  | ForwardDecl { name :: Id,
                  ret :: Type,
                  argsTypes :: [Type] }
  | FuncDef { name :: Id,
              ret :: Type,
              args :: [(Type, Id)],
              body :: [Statement]}
  deriving (Show)

data Statement = SBlock [Statement]
               | SVarDecl Type Id
               | SRawExpr Expression
               | SIfThenElse Expression Statement Statement
               | SWhile Expression Statement
               | SReturn Expression
               deriving (Show)

data Expression = EVar Id
                 | EInt Int
                 | EBool Bool
                 | EAdd Expression Expression
                 | ESub Expression Expression
                 | EMul Expression Expression
                 | ELess Expression Expression
                 | EGreater Expression Expression
                 | EEqual Expression Expression
                 | ELessEq Expression Expression
                 | EGreaterEq Expression Expression
                 | ENotEqual Expression Expression
                 | EAnd Expression Expression
                 | EOr Expression Expression
                 | ECall Id [Expression]
                 | EDeref Expression
                 | EAddr Expression
                 | EAssign Expression Expression
                 | EArray Expression Expression
                 | ECast Type Expression
                 deriving (Show, Eq, Ord)
