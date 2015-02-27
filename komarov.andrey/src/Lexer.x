{
module Lexer (
       Token(..), scanTokens
) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol = [\n]

tokens :-
       $eol                     ;
       $white+                  ;
       $digit+                  { \s -> TokenNum (read s) }
       "("                      { \_ -> TokenLParen }
       ")"                      { \_ -> TokenRParen }
       "{"                      { \_ -> TokenLBrace }
       "}"                      { \_ -> TokenRBrace }
       "+"                      { \_ -> TokenAdd }
       "-"                      { \_ -> TokenSub }
       "*"                      { \_ -> TokenMul }
       "/"                      { \_ -> TokenDiv }
       "%"                      { \_ -> TokenMod }
       "<"                      { \_ -> TokenLess }
       ">"                      { \_ -> TokenGreater }
       "=="                     { \_ -> TokenEqual }
       "<="                     { \_ -> TokenLessEq }
       ">="                     { \_ -> TokenGreaterEq }
       "!="                     { \_ -> TokenNotEqual }
       "&&"                     { \_ -> TokenAnd }
       "||"                     { \_ -> TokenOr }
       "="                      { \_ -> TokenAssign }
       ";"                      { \_ -> TokenSemicolon }
       "if"                     { \_ -> TokenIf }
       "while"                  { \_ -> TokenWhile }
       "return"                 { \_ -> TokenReturn }
       $alpha+                  { \s -> TokenVar s }

{

data Token = TokenNum Int
           | TokenVar String
           | TokenLParen
           | TokenRParen
           | TokenLBrace
           | TokenRBrace
           | TokenAdd
           | TokenSub
           | TokenMul
           | TokenDiv
           | TokenMod
           | TokenLess
           | TokenGreater
           | TokenEqual
           | TokenLessEq
           | TokenGreaterEq
           | TokenNotEqual
           | TokenAnd
           | TokenOr
           | TokenAssign
           | TokenSemicolon
           | TokenIf
           | TokenWhile
           | TokenReturn
           deriving (Eq, Show)

scanTokens = alexScanTokens

}
