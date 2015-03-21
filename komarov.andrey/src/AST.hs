module AST (
  Id, Type,
  Program(..),
  FunctionDefinition(..),
  Statement(..),
  Expression(..)
  ) where

type Id = String

type Type = String

data Program = Program [FunctionDefinition]
               deriving (Show, Eq, Ord)

data FunctionDefinition = FunctionDefinition
                          { functionRetType :: Type
                          , functionName    :: Id
                          , functionArgs    :: [(Type, Id)]
                          , functionBody    :: [Statement]
                          } deriving (Show, Eq, Ord)

data Statement = Block [Statement]
               | VariableDeclaration Type [Id]
               | Assignment Id Expression
               | RawExpression Expression
               | IfThenElse Expression Statement Statement
               | While Expression Statement
               | Return Expression
               deriving (Show, Eq, Ord)

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
