module CPUDefs where

import Data.Int
import Data.Word
import Data.Array.Unboxed
import Control.Monad.State.Strict
import Numeric (showHex)

data CPU = CPU {
   aReg     :: Operand,
   xReg     :: Operand,
   yReg     :: Operand,
   sp       :: Word8,
   pc       :: Address,
   status   :: Status,
   mem      :: Memory
}

instance Show CPU where
    show cpu = " A=" ++ toHex (aReg cpu)
            ++ " | X=" ++ toHex (xReg cpu)
            ++ " | Y=" ++ toHex (yReg cpu)
            ++ " | PC=" ++ toHex (pc cpu)
            ++ " | SP=" ++ toHex (sp cpu)
            ++ " | " ++ show (status cpu)
--            ++ " | Stack "++ show [ (i, (lowMem $ mem cpu) ! i) | i <- [0x100..0x1FF]]
            ++ " |\n"
        where
          toHex m = showHex (fromIntegral m :: Word16) "h"

data Status = Status {
   c  :: Bool,
   z  :: Bool,
   i  :: Bool,
   d  :: Bool,
   b  :: Bool,
   v  :: Bool,
   n  :: Bool,
   q  :: Bool
} --deriving Show

instance Show Status where
    show status = "c:"  ++ show (toInt (c status))
               ++ ",z:" ++ show (toInt (z status))
               ++ ",i:" ++ show (toInt (i status))
               ++ ",d:" ++ show (toInt (d status))
               ++ ",b:" ++ show (toInt (b status))
               ++ ",v:" ++ show (toInt (v status))
               ++ ",n:" ++ show (toInt (n status))
        where
          toInt f = if f then 1 else 0

type NES a = State CPU a

baseSP :: Address
baseSP = 0x0100

data Memory = Memory {
    lowMem      :: !MemArray,   -- 0    - 7FF
    ppu         :: !MemArray,   -- 2000 - 2007
    uppMem      :: !MemArray,   -- 4000 - FFFF
    dma         :: Operand      -- 4014
}

type MemArray   = UArray Address Operand
type Operand    = Int8
type OPCode     = Int8
type Address    = Word16
data Index      = X | Y

-- Before = X, After = Y (apparently)
data Indexed = Before | After

data AddrMode = 
     ZeroPage (Maybe Index) Operand
   | Absolute (Maybe Index) Address
   | Relative Operand
   | Indirect Address
   | Indexed Indexed Operand
