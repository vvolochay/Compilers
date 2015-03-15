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
       "else"                   { \_ -> TokenElse }
       "while"                  { \_ -> TokenWhile }
       "return"                 { \_ -> TokenReturn }
       "true"                   { \_ -> TokenTrue }
       "false"                  { \_ -> TokenFalse }
       ","                      { \_ -> TokenComma }
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
           | TokenElse
           | TokenWhile
           | TokenReturn
           | TokenTrue
           | TokenFalse
           | TokenComma
           deriving (Eq, Show)

scanTokens = alexScanTokens

}
