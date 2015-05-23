module FCC.Eval (
  Eval(..),
  Value(..),
  ) where

import Data.Int
import qualified Data.Map as M

data Value
  = VVoid
  | VInt Int32
  | VBool Bool
  | VArray (M.Map Int32 Value)
    deriving (Eq, Ord, Show, Read)

type Eval = [Value] -> Maybe Value

