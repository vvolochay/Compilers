{
module FCC.Parser (
       parse
) where

import FCC.Lexer
import FCC.Expr
import FCC.Program
import FCC.Type

}

%lexer { lexer } { TokenEOF }
%monad { Alex } { >>= } { return }
%name parseAlex
%tokentype { Token }
%error { parseError }

%token
        '('             { TokenLParen }
        ')'             { TokenRParen }
        '{'             { TokenLBrace }
        '}'             { TokenRBrace }
        '['             { TokenLBracket }
        ']'             { TokenRBracket }
        '+'             { TokenAdd }
        '-'             { TokenSub }
        '*'             { TokenMul }
        '!'             { TokenNot }
        '<'             { TokenLess }
        '>'             { TokenGreater }
        '=='            { TokenEqual }
        '<='            { TokenLessEq }
        '>='            { TokenGreaterEq }
        '!='            { TokenNotEqual }
        '&&'            { TokenAnd }
        '||'            { TokenOr }
        '^'             { TokenXor }
        '='             { TokenAssign }
        ';'             { TokenSemicolon }
        if              { TokenIf }
        else            { TokenElse }
        while           { TokenWhile }
        return          { TokenReturn }
        num             { TokenNum $$ }
        true            { TokenTrue }
        false           { TokenFalse }
        var             { TokenVar $$ }
        tyvar           { TokenTyVar $$ }
        ','             { TokenComma }

%left ','
%right '='
%left '||' '&&'
%left '^'
%nonassoc '==' '!='
%nonassoc '<' '>' '<=' '>='
%left '+' '-'
%left '*'
%left '!'
%nonassoc '[' ']'


%%

Prog            :: { Program String }
Prog            : TopLevels                     { program $1 }

Expr            :: { Expr String }
Expr            : var                           { Var $1 }
                | num                           { Lit $1 }
                | true                          { LitBool True }
                | false                         { LitBool False }
                | '(' Expr ')'                  { $2 }
                | Expr '+' Expr                 { Call (Var "_builtin_add") [$1, $3] }
                | Expr '-' Expr                 { Call (Var "_builtin_sub") [$1, $3] }
                | Expr '*' Expr                 { Call (Var "_builtin_mul") [$1, $3] }
                | Expr '||' Expr                { Call (Var "_builtin_or") [$1, $3] }
                | Expr '&&' Expr                { Call (Var "_builtin_and") [$1, $3] }
                | Expr '^' Expr                 { Call (Var "_builtin_xor") [$1, $3] } 
                | '!' Expr                      { Call (Var "_builtin_not") [$2] }
                | Expr '<' Expr                 { Call (Var "_builtin_less") [$1, $3] }
                | Expr '<=' Expr                { Call (Var "_builtin_lesseq") [$1, $3] }
                | Expr '>' Expr                 { Call (Var "_builtin_greater") [$1, $3] }
                | Expr '>=' Expr                { Call (Var "_builtin_greatereq") [$1, $3] }
                | Expr '==' Expr                { Eq $1 $3 }
                | Expr '!=' Expr                { Call (Var "_builtin_not") [Eq $1 $3] }
                | var '(' FunCallList ')'       { Call (Var $1) $3 }
                | Expr '[' Expr ']'             { Array $1 $3 }
                | Expr '=' Expr                 { Assign $1 $3 }
                | '{' Stmts '}'                 { $2 }

Stmt            :: { Expr String }
                : Expr ';'                      { $1 }
                | if '(' Expr ')' '{' Stmts '}' else '{' Stmts '}' { If $3 $6 $10 }
                | while '(' Expr ')' '{' Stmts '}' { While $3 $6 }
                | return Expr ';'               { Return $2 }

Stmts           :: { Expr String }
Stmts           : {- empty -}                   { Empty }
                | Stmt Stmts                    { Seq $1 $2 }
                | Type var ';' Stmts            { declVar $1 $2 $4 }

FunCallList    :: { [Expr String] }
FunCallList    : {- empty -}                   { [] }
                | Expr                          { [$1] }
                | Expr ',' FunCallList         { $1:$3 }

Type            :: { Type }
Type            : tyvar                         { toPrimitiveType $1 }
                | Type '*'                      { TArray $1 }

TopLevel        :: { TopLevel String }
TopLevel        : Type var ';'                  { DeclVar $1 $2 }
                | Type var '(' FunArgsList ')' '{' Stmts '}' { DeclFun $2 $1 $4 $7 }

FunArgsList     :: { [(String, Type)] }
FunArgsList     : {- empty -}                   { [] }
                | Type var                      { [($2, $1)] }
                | Type var ',' FunArgsList      { ($2, $1):$4 }

TopLevels       :: { [TopLevel String] }
TopLevels       : {- empty -}                   { [] }
                | TopLevel TopLevels            { $1:$2 }

{
parseError :: Token -> Alex a
parseError t = alexError $ "Parse error on token " ++ show t

parse :: String -> Either String (Program String)
parse s = runAlex s parseAlex

}
