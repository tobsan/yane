module Types where

import Data.Int
import Data.Word

type Operand    = Int8
type OPCode     = Int8
type Address    = Word16

data Action
    = Read Address
    | Write Address Operand
    | DMA [Operand]
    | OAMData Operand
    | PPUData Operand
    | NMIIRQ
    | NOP
  deriving Show
