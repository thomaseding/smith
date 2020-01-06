module Tokens (
    Operator(..)
  , Macro(..)
  , Register(..)
  , Operand(..)
  ) where

import Prelude ()
import Prelude'
import Data.List (concat)

-----------------------------------------------------------

-- Define what happens with the TTY cases not presented in the specs, such as (MOV TTY "abc")

data Operator
  = MOV Register (Either Operand String)
  | SUB Register Operand
  | MUL Register Operand
  | NOT Register
  | COR Operand Operand Operand -- ???
  | BLA Operand Operator Operand -- ???
  | NOP
  | STOP
  deriving (Eq)

instance Show Operator where
  show (MOV r ei) = concat ["MOV ", show r, ",", either show show ei]
  show (SUB r op) = concat ["SUB ", show r, ",", show op]
  show (MUL r op) = concat ["MUL ", show r, ",", show op]
  show (NOT r) = concat ["NOT ", show r]
  show (COR i op1 op2) = concat ["COR ", show i, ",", show op1, ",", show op2]
  show (BLA op1 opr op2) = concat ["COR ", show op1, ",", show opr, ",", show op2]
  show NOP = "NOP"
  show STOP = "STOP"

data Macro
  = REP Integer Operator
  deriving (Eq)

instance Show Macro where
  show (REP i op) = "REP " ++ show i ++ " " ++ show op

data Register 
  = REG Integer
  | IREG Register
  | TTY
  | PC
  deriving (Eq)

instance Show Register where
  show (REG n) = "R" ++ show n
  show (IREG r) = "R[" ++ show r ++ "]"
  show TTY = "TTY"
  show PC = "PC"

data Operand
  = Reg Register
  | Immed Integer
  | CurrLine
  deriving (Eq)

instance Show Operand where
  show (Reg r) = show r
  show (Immed i) = show i
  show CurrLine = "*"
