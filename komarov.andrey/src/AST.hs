module AST (
  Id, Type,
  Program(..),
  FunctionDefinition(..),
  Statement(..),
  Expresstion(..)
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
               | Assignment Id Expresstion
               | RawExpression Expresstion
               | IfThen Expresstion Statement
               | IfThenElse Expresstion Statement Statement
               | While Expresstion Statement
               | Return Expresstion
               deriving (Show, Eq, Ord)

data Expresstion = EVar Id
                 | EInt Int
                 | EBool Bool
                 | EAdd Expresstion Expresstion
                 | ESub Expresstion Expresstion
                 | EMul Expresstion Expresstion
                 | EDiv Expresstion Expresstion
                 | EMod Expresstion Expresstion
                 | ELess Expresstion Expresstion
                 | EGreater Expresstion Expresstion
                 | EEqual Expresstion Expresstion
                 | ELessEq Expresstion Expresstion
                 | EGreaterEq Expresstion Expresstion
                 | ENotEqual Expresstion Expresstion
                 | EAnd Expresstion Expresstion
                 | EOr Expresstion Expresstion
                 | ECall Id [Expresstion]
                 deriving (Show, Eq, Ord)
