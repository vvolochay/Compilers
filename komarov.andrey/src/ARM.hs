module ARM (
  OpCode(..),
  Cond(..),
  Assembly(..),
  Register(..),
  bp, sp, lr, pc,
  Segment(..),
  AType(..)

  ) where

import Data.Int
   
data Register = R0 | R1 | R2 | R3
              | R4 | R5 | R6 | R7
              | R8 | R9 | R10 | R11
              | R12 | R13 | R14 | R15
  deriving (Eq, Ord, Show)

bp, sp, lr, pc :: Register
bp = R11
sp = R13
lr = R14
pc = R15

data Cond = Eq -- Z set
          | Ne -- Z clear
          | Cs -- C set
          | Cc -- C clear
          | Mi -- N set
          | Pl -- N clear
          | Vs -- V set
          | Vc -- V clear
          | Hi -- Cs & Zc
          | Ls -- Cc | Zs
          | Ge -- (Ns & Vs) | (Nc & Vc)
          | Lt -- (Ns & Vc) | (Nc & Vs)
          | Gt -- Zc & Ge
          | Le -- Zs & Lt
          | Al -- always
          | Nv -- reserved
          deriving (Show)

data SetFlags = Update | Ignore
              deriving (Show)

-- TODO добавить barrel shifter
data Operand2 = Reg Register | Imm Int32
              deriving (Show)

data OpCode
  = ADD SetFlags Register Register Operand2
  | SUB SetFlags Register Register Operand2
  | RSB SetFlags Register Register Operand2
  | MUL SetFlags Register Register Operand2
  | B Int32
  | BL Int32
  | CMP Register Operand2
  | CMN Register Operand2
  | TST Register Operand2
  | TEQ Register Operand2 -- xor
  | AND SetFlags Register Register Operand2
  | EOR SetFlags Register Register Operand2
  | ORR SetFlags Register Register Operand2
  | BIC SetFlags Register Register Operand2 -- and not
  | MOV SetFlags Register Operand2
  | MVN SetFlags Register Operand2
  | LDR' Register Int32 -- cheats
  | LDR Register Register Operand2
  | STR Register Register Operand2
  | SWI
  deriving (Show)

data Segment = Data | Text
             deriving (Show, Eq, Ord)

data AType = Word
           deriving (Show)

data Assembly
  = OpCode Cond OpCode
  | Label String
  | Raw AType String
  | Comment String
  | EmptyLine
  deriving (Show)
