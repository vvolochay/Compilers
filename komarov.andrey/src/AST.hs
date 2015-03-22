module AST (
  Id,
  Program(..),
  TopLevel(..),
  Statement(..),
  Expression(..)
  ) where

type Id = String

data Program = Program [TopLevel]
          deriving (Show)

data TopLevel
  = VarDecl Id Id
  | ForwardDecl { name :: Id,
                  ret :: Id,
                  argsTypes :: [Id] }
  | FuncDef { name :: Id,
              ret :: Id,
              args :: [(Id, Id)],
              body :: [Statement]}
  deriving (Show)

data Statement = SBlock [Statement]
               | SVarDecl Id Id
               | SAssignment Id Expression
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
                 deriving (Show, Eq, Ord)
