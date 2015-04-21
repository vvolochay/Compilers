{
module Parser (
       parse
) where

import Lexer
import AST

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
%left '&' DEREF CAST
%nonassoc '[' ']'


%%

Prog            :: { Program () }
Prog            : TopLevelDefs                  { Program $1 }

TopLevelDefs    :: { [TopLevel ()] }
TopLevelDefs    : {- empty -}                   { [] }
                | TopLevel TopLevelDefs         { $1:$2 }

TopLevel        :: { TopLevel () }
TopLevel        : Type var ';'                  { VarDecl $1 $2 }
                | Type var '(' FuncArgs ')' ';' { ForwardDecl $2 $1 (map fst $4) }
                | Type var '(' FuncArgs ')' '{' Stmts '}' { FuncDef $2 $1 $4 $7 }

Expr            :: { Expression () }
Expr            : var                           { EVar $1 }
                | num                           { ELitInt $1 }
                | true                          { ELitBool True }
                | false                         { ELitBool False }
                | '(' Expr ')'                  { $2 }
                | Expr '+' Expr                 { EArith AddOp (notag $1) (notag $3) }
                | Expr '-' Expr                 { EArith SubOp (notag $1) (notag $3) }
                | Expr '*' Expr                 { EArith MulOp (notag $1) (notag $3) }
                | Expr '||' Expr                { EBool OrOp (notag $1) (notag $3) }
                | Expr '&&' Expr                { EBool AndOp (notag $1) (notag $3) }
                | Expr '^' Expr                 { EBool XorOp (notag $1) (notag $3) }
                | Expr '<' Expr                 { EArithCmp LessOp (notag $1) (notag $3) }
                | Expr '<=' Expr                { EArithCmp LessEqOp (notag $1) (notag $3) }
                | Expr '>' Expr                 { EArithCmp GreaterOp (notag $1) (notag $3) }
                | Expr '>=' Expr                { EArithCmp GreaterEqOp (notag $1) (notag $3) }
                | Expr '==' Expr                { EEqual EqOp (notag $1) (notag $3) }
                | Expr '!=' Expr                { EEqual NeqOp (notag $1) (notag $3) }
                | var '(' FuncCallList ')'      { ECall $1 (map notag $3) }
                | '&' Expr                      { EAddr (notag $2) }
                | '*' Expr %prec DEREF          { EDeref (notag $2) }
                | Expr '[' Expr ']'             { EArray (notag $1) (notag $3) }
                | Expr '=' Expr                 { EAssign (notag $1) (notag $3) }
                | '(' Type ')' Expr %prec CAST  { ECast $2 (notag $4) }

FuncCallList    :: { [Expression ()] }
FuncCallList    : {- empty -}                   { [] }
                | Expr                          { [$1] }
                | Expr ',' FuncCallList         { $1:$3 }

Stmt            :: { Statement () }
Stmt            : '{' Stmts '}'                 { SBlock $2 }
                | Type var ';'                  { SVarDecl $1 $2 }
                | Expr ';'                      { SRawExpr (notag $1) }
                | if '(' Expr ')' Stmt else Stmt  { SIfThenElse (notag $3) $5 $7 }
                | while '(' Expr ')' Stmt       { SWhile (notag $3) $5 }
                | return Expr ';'               { SReturn (notag $2) }

Stmts           :: { [Statement ()] }
Stmts           : {- empty -}                   { [] }
                | Stmt Stmts                    { $1:$2 }

FuncArgs        :: { [(Type, Id)] }
FuncArgs        : {- empty -}                   { [] }
                | Type var                      { [($1, $2)] }
                | Type var ',' FuncArgs         { ($1, $2):$4 }

Type            :: { Type }
Type            : tyvar                         { case $1 of { "int" -> TInt; "bool" -> TBool; n -> error $ "INTERNAL COMPILER ERROR" ++ n } }
                | Type '*'                      { TPointer $1 }

{
parseError :: Token -> Alex a
parseError t = alexError $ "Parse error on token " ++ show t

parse :: String -> Either String (Program ())
parse s = runAlex s parseAlex
}
