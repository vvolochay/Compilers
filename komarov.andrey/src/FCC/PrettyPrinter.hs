{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FCC.PrettyPrinter (
  ) where

import Control.Monad.RWS

import FCC.AST

type Offset = Int
type Output = [String]
newtype Config = Config { offsetStep :: Int }

defaultConfig = Config 4

newtype PrettyPrinter a = PrettyPrinter {
  runPrettyPrinter :: RWS Config Output Offset a
  } deriving (
  Functor, Applicative, Monad, MonadState Offset, MonadWriter Output, MonadReader Config
  )

ppline :: String -> PrettyPrinter ()
ppline s = do
  off <- get
  tell [(replicate off ' ') ++ s]

scoped :: PrettyPrinter a -> PrettyPrinter a
scoped p = do
  add <- asks offsetStep
  modify (+add)
  res <- p
  modify (`subtract` add)
  return res

class PrettyPrintable t where
  pprint :: t -> PrettyPrinter ()

