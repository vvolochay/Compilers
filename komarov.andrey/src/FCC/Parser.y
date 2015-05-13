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
        '&'             { TokenAmp }
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
%left '&' DEREF CAST '!'
%nonassoc '[' ']'


%%

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
                | Expr '<' Expr                 { Call (Var "_builtin_less") [$1, $3] }
                | Expr '<=' Expr                { Call (Var "_builtin_lesseq") [$1, $3] }
                | Expr '>' Expr                 { Call (Var "_builtin_greater") [$1, $3] }
                | Expr '>=' Expr                { Call (Var "_builtin_greatereq") [$1, $3] }
                | Expr '==' Expr                { Eq $1 $3 }
                | Expr '!=' Expr                { Call (Var "_builtin_not") [Eq $1 $3] }
                | var '(' FuncCallList ')'      { Call (Var $1) $3 }
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

FuncCallList    :: { [Expr String] }
FuncCallList    : {- empty -}                   { [] }
                | Expr                          { [$1] }
                | Expr ',' FuncCallList         { $1:$3 }

Type            :: { Type }
Type            : tyvar                         { toPrimitiveType $1 }
                | Type '*'                      { TArray $1 }

{-

Prog            :: { Program () }
Prog            : TopLevelDefs                  { Program $1 }

TopLevelDefs    :: { [TopLevel ()] }
TopLevelDefs    : {- empty -}                   { [] }
                | TopLevel TopLevelDefs         { $1:$2 }

TopLevel        :: { TopLevel () }
TopLevel        : Type var ';'                  { VarDecl $1 $2 }
                | Type var '(' FuncArgs ')' ';' { ForwardDecl $2 $1 (map fst $4) }
                | Type var '(' FuncArgs ')' '{' Stmts '}' { FuncDef $2 $1 $4 (SBlock $7) }




FuncArgs        :: { [(Type, Id)] }
FuncArgs        : {- empty -}                   { [] }
                | Type var                      { [($1, $2)] }
                | Type var ',' FuncArgs         { ($1, $2):$4 }

 -}

{
parseError :: Token -> Alex a
parseError t = alexError $ "Parse error on token " ++ show t

parse :: String -> Either String (Expr String)
parse s = runAlex s parseAlex
}
