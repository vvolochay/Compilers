{
module Parser (
       parse
) where

import Lexer
import AST

}

%name parse
%tokentype { Token }
%error { parseError }

%token
        '('             { TokenLParen }
        ')'             { TokenRParen }
        '{'             { TokenLBrace }
        '}'             { TokenRBrace }
        '+'             { TokenAdd }
        '-'             { TokenSub }
        '*'             { TokenMul }
        '/'             { TokenDiv }
        '%'             { TokenMod }
        '<'             { TokenLess }
        '>'             { TokenGreater }
        '=='            { TokenEqual }
        '<='            { TokenLessEq }
        '>='            { TokenGreaterEq }
        '!='            { TokenNotEqual }
        '&&'            { TokenAnd }
        '||'            { TokenOr }
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
        ','             { TokenComma }

%left ','
%left '||' '&&'
%left '<' '>' '<=' '>=' '==' '!='
%left '+' '-'
%left '*' '/' '%'

%%

Prog            : FuncDefs                      { Program $1 }

Expr            : var                           { EVar $1 }
                | num                           { EInt $1 }
                | true                          { EBool True }
                | false                         { EBool False }
                | '(' Expr ')'                  { $2 }
                | Expr '+' Expr                 { EAdd $1 $3 }
                | Expr '-' Expr                 { ESub $1 $3 }
                | Expr '*' Expr                 { EMul $1 $3 }
                | Expr '/' Expr                 { EDiv $1 $3 }
                | Expr '%' Expr                 { EMod $1 $3 }
                | Expr '<' Expr                 { ELess $1 $3 }
                | Expr '>' Expr                 { EGreater $1 $3 }
                | Expr '==' Expr                { EEqual $1 $3 }
                | Expr '<=' Expr                { ELessEq $1 $3 }
                | Expr '>=' Expr                { EGreaterEq $1 $3 }
                | Expr '!=' Expr                { ENotEqual $1 $3 }
                | Expr '&&' Expr                { EAnd $1 $3 }
                | Expr '||' Expr                { EOr $1 $3 }
                | var '(' FuncCallList ')'      { ECall $1 $3 }

FuncCallList    : {- empty -}                   { [] }
                | Expr                          { [$1] }
                | Expr ',' FuncCallList         { $1:$3 }

Stmt            : '{' Stmts '}'                 { Block $2 }
                | var Vars ';'                  { VariableDeclaration $1 $2 }
                | var '=' Expr ';'              { Assignment $1 $3}
                | Expr ';'                      { RawExpression $1 }
                | if '(' Expr ')' Stmt else Stmt  { IfThenElse $3 $5 $7 }
                | while '(' Expr ')' Stmt       { While $3 $5 }
                | return Expr ';'               { Return $2 }

Vars            : var                           { [$1] }
                | var ',' Vars                  { $1:$3 }

Stmts           : {- empty -}                   { [] }
                | Stmt Stmts                { $1:$2 }

FuncDef         : var var '(' FuncArgs ')' '{' Stmts '}'        { FunctionDefinition $1 $2 $4 $7 }

FuncArgs        : {- empty -}                   { [] }
                | var var                       { [($1, $2)] }
                | var var ',' FuncArgs          { ($1, $2):$4 }

FuncDefs        : FuncDef                       { [$1] }
                | FuncDef FuncDefs              { $1:$2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
