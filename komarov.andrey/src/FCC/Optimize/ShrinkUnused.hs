module FCC.Optimize.ShrinkUnused (
  shrink,
  ) where

import FCC.Program

import Data.Foldable
import Control.Monad.RWS

import qualified Data.Set as S
import qualified Data.Map as M

start = "_start"

shrink :: Program String -> Program String
shrink p@(Program funs vars) = p' where
  used = fix' (S.singleton start) (upd funs)
  funs' = M.filterWithKey (\n _ -> n `S.member` used) funs
  vars' = M.filterWithKey (\n _ -> n `S.member` used) vars
  p' = Program funs' vars'
   
fix' :: Eq a => a -> (a -> a) -> a
fix' init mod = if new == init then init else fix' new mod where
  new = mod init

upd :: M.Map String (Function String) -> S.Set String -> S.Set String
upd funs reached = reached `S.union` S.unions [free f | (name, f) <- M.toList funs, name `S.member` reached]

free :: Function String -> S.Set String
free (Function _ _ (Native _ _)) = S.empty
free (Function _ _ (Inner s)) = S.fromList $ toList s
  
