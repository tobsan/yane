module CPUDefs where

import Types
import Control.Applicative

class (Monad m, Applicative m) => NesCPU m where
    -- * Get registers.
    getA, getX, getY, getStatus :: m Operand
    getPC :: m Address
    getSP :: m Word8

    -- * Set registers.
    setA, setX, setY, setStatus :: Operand -> m ()
    setPC :: Address -> m ()
    setSP :: Word8 -> m ()

    -- * Alter registers and return the new value.
    alterA, alterX, alterY :: (Operand -> Operand) -> m Operand
    alterStatus :: (Operand -> Operand) -> m () -- notice!
    alterPC :: (Address -> Address) -> m Address
    alterSP :: (Word8 -> Word8) -> m Word8

    -- * Memory.
    readMemory  :: Address -> m Operand
    writeMemory :: Address -> Operand -> m ()
    alterMemory :: Address -> (Operand -> Operand) -> m Operand

    -- * Flags.
    getFlagC, getFlagZ, getFlagI, getFlagD, getFlagB, getFlagQ, getFlagV
      , getFlagN :: m Bool
    setFlagC, setFlagZ, setFlagI, setFlagD, setFlagB, setFlagQ, setFlagV
      , setFlagN :: Bool -> m ()

    -- * Interrupt handler.
    setIRQ :: (Maybe IRQ) -> m ()
    getIRQ :: m (Maybe IRQ)

    -- * Enterprise pulling.
    handlePPUAction :: [Action] -> m ()  -- ^ Handle actions from PPU.
    getCPUAction    :: m Action          -- ^ Recieve the action from CPU.
    setCPUAction    :: Action -> m ()    -- ^ Set the given action in CPU.

-- | The stack's starting position.
baseSP :: Address
baseSP = 0x0100
