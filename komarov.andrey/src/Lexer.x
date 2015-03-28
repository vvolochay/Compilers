{
module Lexer (
       Token(..), scanTokens
) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$alnum = [a-zA-Z0-9_]
$eol = [\n]

tokens :-
       $eol                     ;
       $white+                  ;
       $digit+                  { \s -> TokenNum (read s) }
       "("                      { \_ -> TokenLParen }
       ")"                      { \_ -> TokenRParen }
       "{"                      { \_ -> TokenLBrace }
       "}"                      { \_ -> TokenRBrace }
       "["                      { \_ -> TokenLBracket }
       "]"                      { \_ -> TokenRBracket }
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
       "&"                      { \_ -> TokenAmp }
       $alpha $alnum*           { \s -> TokenVar s }

{

data Token = TokenNum Int
           | TokenVar String
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
           | TokenAmp
           deriving (Eq, Show)

scanTokens = alexScanTokens

}
