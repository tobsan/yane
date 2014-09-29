module CPUDefs where

import Data.Int
import Data.Word

import Data.STRef
import Data.Array.ST
import Control.Monad.ST
import Control.Monad.Reader

--import Control.Monad.State.Strict
import Numeric (showHex)

data Sys s = Sys {
    cpu :: STRef s CPU,
    mem :: STRef s (Memory s)
}

data CPU = CPU {
    aReg     :: Operand,
    xReg     :: Operand,
    yReg     :: Operand,
    sp       :: Word8,
    pc       :: Address,
    status   :: Status
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

type NES s a = ReaderT (Sys s) (ST s) a

baseSP :: Address
baseSP = 0x0100

type MemArray s  = STUArray s Address Operand
data Memory s = Memory {
    lowMem      :: MemArray s,   -- 0    - 7FF
    ppu         :: MemArray s,   -- 2000 - 2007
    uppMem      :: MemArray s   -- 4000 - FFFF
--    dma         :: Operand       -- 4014
}

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
