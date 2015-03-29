{
module Lexer (
       Alex(..), runAlex,
       Token(..), lexer
) where

import ParserMonad
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
       $alpha $alnum*           { \(_, _, _, s) l -> return $ TokenVar $ take l s }

{

r :: Token -> AlexInput -> Int -> Alex Token
r t _ _ = return t

data AlexUserState = AlexUserState { wtf :: Int }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0

alexEOF :: Alex Token
alexEOF = return TokenEOF

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
           | TokenEOF
           deriving (Eq, Show)

lexer :: (Token -> Alex a) -> Alex a
lexer cont = (alexMonadScan >>= cont)

}
