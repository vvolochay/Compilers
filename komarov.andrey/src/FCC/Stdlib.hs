module FCC.Stdlib (
  builtins,
  withStdlib
  ) where

import FCC.Type
import FCC.Expr
import FCC.Program

import Bound

import qualified Data.Map as M

withStdlib :: Program String -> Program String
withStdlib (Program funs vars) = Program (funs `M.union` (M.fromList builtins)) vars

native :: [Type] -> Type -> [String] -> Function String
native args ret body = Function args ret $ Native body

start :: Function String
start = Function [] TInt $ Inner $ abstract (const Nothing) $ Call (Var "_exit") [Call (Var "main") []]

exit :: Function String
exit = native [TInt] TVoid ["pop {r0}", "mov r7, #1", "swi 0"]

new :: Function String
new = native [TVoid] (TArray TVoid) ["ldr r0, =0", "pop {r1}", "ldr r2, =3",
                                     "ldr r3, =33", "ldr r4, =0", "ldr r5, =0",
                                     "ldr r7, =192", "swi 0", "push {r0}"]

builtins :: [(String, Function String)]
builtins = [
  ("_builtin_add", native [TInt, TInt] TInt ["pop {r0}", "pop {r1}", "add r0, r0, r1", "push {r0}"]),
  ("_builtin_sub", native [TInt, TInt] TInt ["pop {r0}", "pop {r1}", "sub r0, r0, r1", "push {r0}"]),
  ("_builtin_mul", native [TInt, TInt] TInt ["pop {r0}", "pop {r1}", "mul r2, r0, r1", "push {r2}"]),
  ("_builtin_less", native [TInt, TInt] TBool ["pop {r1, r2}", "cmp r1, r2", "movlt r0, #1", "movge r0, #0", "push {r0}"]),
  ("_builtin_eq_int", native [TInt, TInt] TBool ["pop {r1, r2}", "teq r1, r2", "moveq r0, #1", "movne r0, #0", "push {r0}"]),
  ("_start", start),
  ("_exit", exit),
  ("_new", new)
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
