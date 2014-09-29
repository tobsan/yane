module Types 
    ( module ST
    , module STRef
    , module STArr
    , module I
    , module W
    , module B
    , module Types
    ) where

import Data.Array.ST as STArr
import Data.Int as I
import Data.Word as W
import Data.Bits as B hiding (bit)

--import Control.Monad.ST.Lazy as ST
--import Data.STRef.Lazy as STR

import Control.Monad.ST as ST
import Data.STRef as STRef

type Operand    = Int8
type OPCode     = Int8
type Address    = Word16
type Pixel      = Word32 -- RGB Value in hex.

type Memory s = STUArray s Address Operand

data Action
    = Read Address
    | Write Address Operand
    | DMA [Operand]
    | NMIIRQ
    | NOP
  deriving (Show, Eq, Ord)

data IRQ
    = Normal -- ^ from some mappers (no support yet).
    | NMI    -- ^ Non-maskable interrupt.
    | Reset  -- ^ ja du fattar.

-- | Cast an Int8 to a Word16 without promotion of sign.
transform :: Operand -> Address
transform op = (fromIntegral op) .&. 0x00FF

data Input      = Input InType Player Key deriving Show
data InType     = KeyPress | KeyRelease deriving Show
data Player     = P1 | P2 deriving (Show,Eq)
data Key        = A | B | Left | Up | Down | Right | Select | Start deriving Show
