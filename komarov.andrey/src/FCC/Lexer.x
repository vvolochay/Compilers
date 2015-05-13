{
module FCC.Lexer (
       Alex(..), runAlex, alexError,
       Token(..), lexer
) where

import qualified Data.Set as S
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]
$alnum = [a-zA-Z0-9_]
$eol = [\n]

tokens :-
       $eol                     ;
       $white+                  ;
       $digit+                  { \(_, _, _, s) l -> return $ TokenNum (read $ take l s) }
       "("                      { r TokenLParen }
       ")"                      { r TokenRParen }
       "{"                      { r TokenLBrace }
       "}"                      { r TokenRBrace }
       "["                      { r TokenLBracket }
       "]"                      { r TokenRBracket }
       "+"                      { r TokenAdd }
       "-"                      { r TokenSub }
       "*"                      { r TokenMul }
       "<"                      { r TokenLess }
       ">"                      { r TokenGreater }
       "=="                     { r TokenEqual }
       "<="                     { r TokenLessEq }
       ">="                     { r TokenGreaterEq }
       "!="                     { r TokenNotEqual }
       "&&"                     { r TokenAnd }
       "||"                     { r TokenOr }
       "^"                      { r TokenXor }
       "="                      { r TokenAssign }
       ";"                      { r TokenSemicolon }
       "if"                     { r TokenIf }
       "else"                   { r TokenElse }
       "while"                  { r TokenWhile }
       "return"                 { r TokenReturn }
       "true"                   { r TokenTrue }
       "false"                  { r TokenFalse }
       ","                      { r TokenComma }
       "&"                      { r TokenAmp }
       "!"                      { r TokenNot }
       $alpha $alnum*           { \(_, _, _, s) l -> var $ take l s }

{

var :: String -> Alex Token
var s = do
  tp <- isType s
  return $ (if tp then TokenTyVar else TokenVar) s

r :: Token -> AlexInput -> Int -> Alex Token
r t _ _ = return t

data AlexUserState = AlexUserState { types :: S.Set String }

isType :: String -> Alex Bool
isType s = do
  AlexUserState {types=t} <- alexGetUserState
  return $ s `S.member` t

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState $ S.fromList ["int", "void", "bool"]

alexEOF :: Alex Token
alexEOF = return TokenEOF

data Token = TokenNum Int
           | TokenVar String
           | TokenTyVar String
           | TokenLParen
           | TokenRParen
           | TokenLBrace
           | TokenRBrace
           | TokenLBracket
           | TokenRBracket
           | TokenAdd
           | TokenSub
           | TokenMul
           | TokenLess
           | TokenGreater
           | TokenEqual
           | TokenLessEq
           | TokenGreaterEq
           | TokenNotEqual
           | TokenNot
           | TokenAnd
           | TokenOr
           | TokenXor
           | TokenAssign
           | TokenSemicolon
           | TokenIf
           | TokenElse
           | TokenWhile
           | TokenReturn
           | TokenTrue
           | TokenFalse
           | TokenComma
           | TokenAmp
           | TokenEOF
           deriving (Eq, Show)

lexer :: (Token -> Alex a) -> Alex a
lexer cont = (alexMonadScan >>= cont)

}
