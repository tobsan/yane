{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CPUState
    ( CPUState
    ) where

import CPUDefs
import Types hiding (Memory)
import Control.Monad.State.Strict
import Data.Array.Unboxed

newtype CPUState a = CPUState
    { runCPUState :: State CPUEnv a 
    } deriving (Monad, MonadState CPUEnv)

type MemArray   = UArray Address Operand

data CPUEnv = CPUEnv {
   aReg     :: Operand,
   xReg     :: Operand,
   yReg     :: Operand,
   sp       :: Operand,
   pc       :: Address,
   status   :: Status,
   mem      :: Memory
}

data Status = Status {
   c  :: Bool,
   z  :: Bool,
   i  :: Bool,
   d  :: Bool,
   b  :: Bool,
   v  :: Bool,
   n  :: Bool
} deriving Show

data Memory = Memory {
    lowMem      :: !MemArray,   -- 0    - 7FF
    ppu         :: !MemArray,   -- 2000 - 2007
    uppMem      :: !MemArray,   -- 4000 - FFFF
    dma         :: Operand      -- 4014
}

instance NesCPU CPUState where
    getA = gets aReg
    setA a = get >>= \cpu -> put $ cpu { aReg = a }
    alterA f = getA >>= setA . f >> getA

    getX = gets xReg
    setX a = get >>= \cpu -> put $ cpu { xReg = a }
    alterX f = getX >>= setX . f >> getX

    getY = gets yReg
    setY a = get >>= \cpu -> put $ cpu { yReg = a }
    alterY f = getY >>= setY . f >> getY
    -- ... and so forth

