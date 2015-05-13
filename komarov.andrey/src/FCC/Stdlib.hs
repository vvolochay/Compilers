module FCC.Stdlib (
  builtins
  ) where

import FCC.Type
import FCC.Expr
import FCC.Program

native :: [Type] -> Type -> [String] -> Function String
native args ret body = Function args ret $ Native body

builtins :: [(String, Function String)]
builtins = [
  ("_builtin_add", native [TInt, TInt] TInt ["pop r0", "pop r1", "add r0, r0, r1", "push r0"])
 ]

           {-
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
                | Expr '!=' Expr                { Call (Var "_builtin_not") (Eq $1 $3) }
-}
