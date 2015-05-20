module FCC.Eval (
  Eval(..),
  Value(..),
  ) where

import FCC.Expr

import Data.Int
import qualified Data.Map as M

data Value
  = VVoid
  | VInt Int32
  | VBool Bool
  | VArray (M.Map Int32 Value)

type Eval = [Value] -> Maybe Value

