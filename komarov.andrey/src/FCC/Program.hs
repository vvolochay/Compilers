module FCC.Program (
  Function(..),
  Program(..),
  TopLevel(..),
  FunctionBody(..),
  function,
  program,
  ) where

import Data.List (elemIndex)
import Bound

import FCC.Expr
import FCC.Type

import qualified Data.Map as M

data FunctionBody a
  = Inner (Scope Int Expr a)
  | Native [String]
  deriving (Eq, Ord, Show)

data Function a
  = Function { argsTypes :: [Type], retType :: Type, body :: FunctionBody a }
  deriving (Eq, Ord, Show)

data Program a =
  Program { functions :: M.Map a (Function a), variables :: M.Map a Type }
  deriving (Eq, Ord, Show)

function :: Eq a => [(a, Type)] -> Type -> Expr a -> Function a
function args ret body = Function (map snd args) ret $
                         Inner $ abstract (`elemIndex` (map fst args)) body

data TopLevel a
  = DeclVar Type a
  | DeclFun a Type [(a, Type)] (Expr a)

-- TODO перестать считать, что всё уникально
program :: Ord a => [TopLevel a] -> Program a
program ts = Program funs vars where
  vars = M.fromList [(a, t) | DeclVar t a <- ts]
  funs = M.fromList [(name, function args ret body) | DeclFun name ret args body <- ts]

