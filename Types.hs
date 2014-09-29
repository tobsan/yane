module Types (module ST, module STR, module I, module W, module B, module Types) where

import Data.Int as I
import Data.Word as W
import Data.Bits as B hiding (bit)

--import Control.Monad.ST.Lazy as STL
--import Data.STRef.Lazy as STRL

import Control.Monad.ST as ST
import Data.STRef as STR

-- Why is this imported?
import Data.Array.Base ()

type Operand    = Int8
type OPCode     = Int8
type Address    = Word16
type Pixel      = Word32 -- | RGB Value in hex.


data Action
    = Read Address
    | Write Address Operand
    | DMA [Operand]
    | NMIIRQ
    | NOP
  deriving (Show, Eq, Ord)

data IRQ
    = Normal -- from some mappers (no support yet).
    | NMI    -- Non-maskable interrupt.
    | Reset  -- ja du fattar.

-- cast an Int8 to a Word16 without promotion of sign
transform :: Operand -> Address
transform op = (fromIntegral op) .&. 0x00FF

data Input      = Input InType Player Key deriving Show
data InType     = KeyPress | KeyRelease deriving Show
data Player     = P1 | P2 deriving (Show,Eq)
data Key        = A | B | Left | Up | Down | Right | Select | Start deriving Show
