module FCC.TypecheckError (
  TypecheckError(..),
  ) where

data TypecheckError
  = SomethingWentWrong
    deriving (Eq, Ord, Show, Read)
