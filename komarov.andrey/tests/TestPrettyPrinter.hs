{-# LANGUAGE TemplateHaskell #-}
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Data.List

import Data.DeriveTH

import FCC.AST

derive makeArbitrary ''EqOp
derive makeArbitrary ''BoolBinOp
derive makeArbitrary ''ArithCmpOp
derive makeArbitrary ''ArithBinOp
derive makeArbitrary ''Type
derive makeArbitrary ''Expression

main = defaultMain tests

tests = [
  testGroup "Expression" [
     testProperty "expr" prop_expr
     ]
  ]

prop_expr :: Expression () -> Bool
prop_expr (EDeref _) = False
prop_expr _ = True
