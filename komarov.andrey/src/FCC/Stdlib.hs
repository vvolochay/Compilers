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

native :: String -> [Type] -> Type -> [String] -> (String, Function String)
native name args ret body = (name, Function args ret $ Native name body)

start :: Function String
start = Function [] TInt $ Inner $ abstract (const Nothing) $ Call (Var "_exit") [Call (Var "main") []]

exit :: (String, Function String)
exit = native "_exit" [TInt] TVoid ["pop {r0}", "mov r7, #1", "swi 0"]

new :: (String, Function String)
new = native "_new" [TVoid] (TArray TVoid) ["ldr r0, =0", "pop {r1}", "add r1, r0, r1, LSL#2", "ldr r2, =3",
                                     "ldr r3, =33", "ldr r4, =0", "ldr r5, =0",
                                     "ldr r7, =192", "swi 0", "push {r0}"]

getchar :: (String, Function String)
getchar = native "getchar" [] TInt ["ldr r7, =3", "ldr r0, =0", "push {r0}", "mov r1, sp", "ldr r2, =1", "swi 0",
                                    "ldr r2, =0", "ldr r1, =-1", "cmp r0, r2", "strle r1, [sp]"]

putchar :: (String, Function String)
putchar = native "putchar" [TInt] TInt ["ldr r7, =4", "ldr r0, =1", "mov r1, sp", "ldr r2, =1", "swi 0",
                                        "ldr r0, [sp]"]

builtins :: [(String, Function String)]
builtins = [
  native "_builtin_add" [TInt, TInt] TInt ["pop {r0}", "pop {r1}", "add r0, r0, r1", "push {r0}"],
  native "_builtin_sub" [TInt, TInt] TInt ["pop {r0}", "pop {r1}", "sub r0, r0, r1", "push {r0}"],
  native "_builtin_mul" [TInt, TInt] TInt ["pop {r0}", "pop {r1}", "mul r2, r0, r1", "push {r2}"],
  native "_builtin_less" [TInt, TInt] TBool ["pop {r1, r2}", "cmp r1, r2", "movlt r0, #1", "movge r0, #0", "push {r0}"],
  native "_builtin_eq_int" [TInt, TInt] TBool ["pop {r1, r2}", "teq r1, r2", "moveq r0, #1", "movne r0, #0", "push {r0}"],
  native "_builtin_not" [TBool] TBool ["pop {r0}", "ldr r1, =1", "sub r0, r1, r0", "push {r0}"],
  ("_start", start),
  getchar,
  putchar,
  exit,
  new
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
