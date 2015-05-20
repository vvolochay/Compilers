module FCC.Optimize.StdlibEval (
  builtinsE
  ) where

import FCC.Eval

import Data.Int
import qualified Data.Map as M

new :: Eval
new [VInt size] = Just $ VArray M.empty

liftI2 :: (Int32 -> Int32 -> Int32) -> Eval
liftI2 op [VInt i1, VInt i2] = Just $ VInt $ i1 `op` i2
liftI2 _ _ = Nothing

liftI2B :: (Int32 -> Int32 -> Bool) -> Eval
liftI2B op [VInt i1, VInt i2] = Just $ VBool $ i1 `op` i2
liftI2B _ _ = Nothing

builtinsE :: M.Map String Eval
builtinsE = M.fromList $ [
  ("_new", new), -- какая-то скользкая дорожка. не доверяю вычислятору new
  ("_builtin_add", liftI2 (+)),
  ("_builtin_sub", liftI2 (-)),
  ("_builtin_mul", liftI2 (*)),
  ("_builtin_less", liftI2B (<)),
  ("_builtin_eq_int", liftI2B (==)),
  ("_builtin_eq_bool", liftI2B (==)),
  ("_builtin_eq_ptr", liftI2B (==))
 ]
  
