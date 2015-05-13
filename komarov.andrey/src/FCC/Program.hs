module FCC.Program (
  Function(..),
  Program(..),
  function,
  ) where

import Data.List (elemIndex)
import Bound

import FCC.Expr
import FCC.Type

import qualified Data.Map as M

data Function a =
  Function { argsTypes :: [Type], retType :: Type, body :: Scope Int Expr a }
  deriving (Eq, Ord, Show)

data Program a =
  Program { functions :: M.Map a (Function a), variables :: M.Map a Type }
  deriving (Eq, Ord, Show)

function :: Eq a => [(a, Type)] -> Type -> Expr a -> Function a
function args ret body = Function (map snd args) ret $
                         abstract (`elemIndex` (map fst args)) body
