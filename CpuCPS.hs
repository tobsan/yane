{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module CpuCPS
    ( CpuCPS
    , runCpuCPS
    , CPUEnv (..)
    , InputState (..)
    ) where

import Types
import CPUDefs
import CPUHelpers ((<#>))

-- import Control.Monad.Reader
import Control.Monad.Cont

import Control.Applicative (Applicative, pure, (<*>), (<$>))

newtype CpuCPS s a = CpuCPS {
      runCpuCPS :: forall r . (a -> ST s r) -> CPUEnv s -> ST s r
    }

instance Monad (CpuCPS s) where
    return x = CpuCPS $ \k _ -> k x
    m >>= f  = CpuCPS $ \k e -> runCpuCPS m (\a -> runCpuCPS (f a) k e) e

instance Functor (CpuCPS s) where 
    -- <$> :: (a -> b) -> CpuCPS s a -> CpuCPS s b
    fmap f m = CpuCPS $ \k e -> runCpuCPS m (k . f) e
--    fmap f m = CpuCPS $ runCpuCPS m . (. f)

instance Applicative (CpuCPS s) where
    -- :: a -> CpuCPS s a
    pure = return
    -- :: CpuCPS s (a -> b) -> CpuCPS s a -> CpuCPS s b
    (<*>) = ap

liftST :: (CPUEnv s -> ST s a) -> CpuCPS s a
liftST m = CpuCPS $ \k e -> m e >>= k

-- | The Environment storing the STRefs.
data CPUEnv s = CPUEnv {
      aReg     :: STRef s Operand
    , xReg     :: STRef s Operand
    , yReg     :: STRef s Operand
    , sp       :: STRef s Word8
    , pc       :: STRef s Address
    , status   :: STRef s Operand
    , irq      :: STRef s (Maybe IRQ)
    , lowMem   :: Memory s             -- ^ Memory range  0    - 7FF
    , ppuMem   :: Memory s             -- ^ Memory range  2000 - 2007
    , uppMem   :: Memory s             -- ^ Memory range  4000 - FFFF
    , action   :: STRef s Action
    , player1  :: STRef s InputState
    , player2  :: STRef s InputState
    }

-- The state that keeps track of user input
data InputState = InputState {
      register :: Operand
    , readNum  :: Int
    }

-- ---------------------------------------------------------------------------
-- Functions for dealing with the environment.

writeArr  :: (CPUEnv s -> Memory s) -> Address -> Operand -> CpuCPS s ()
readArr   :: (CPUEnv s -> Memory s) -> Address -> CpuCPS s Operand
setVar    :: (CPUEnv s -> STRef s a) -> a -> CpuCPS s ()
getVar    :: (CPUEnv s -> STRef s a) -> CpuCPS s a
alterVar  :: (CPUEnv s -> STRef s a) -> (a -> a) -> CpuCPS s a
alterVar_ :: (CPUEnv s -> STRef s a) -> (a -> a) -> CpuCPS s ()

writeArr  r a o = liftST $ \e -> writeArray (r e) a o
readArr   r a   = liftST $ \e -> readArray  (r e) a
setVar    r a   = liftST $ \e -> writeSTRef (r e) a
getVar    r     = liftST $ \e -> readSTRef  (r e)
alterVar  r f   = liftST $ \e -> let !ref = (r e)
                                 in do modifySTRef ref f >> readSTRef ref
alterVar_ r f   = liftST $ \e -> modifySTRef (r e) f

-- ---------------------------------------------------------------------------
-- Functions used later on.

-- | Get bit `x` of status.
getFlagBit :: Int -> CpuCPS s Bool
getFlagBit x = (`testBit` x) <$> getStatus

-- | Set bit `x` of status according to bool.
setFlagBit :: Int -> Bool -> CpuCPS s ()
setFlagBit x b = alterStatus $ if b then (`setBit` x) else (`clearBit` x)

setAction :: Action -> CpuCPS s ()
setAction = setVar action

-- ---------------------------------------------------------------------------
-- | NesCPU instance for the CpuCPS monad

instance NesCPU (CpuCPS s) where
    getA = getVar aReg
    setA = setVar aReg
    alterA = alterVar aReg

    getX = getVar xReg
    setX = setVar xReg
    alterX = alterVar xReg

    getY = getVar yReg
    setY = setVar yReg
    alterY = alterVar yReg

    getPC = getVar pc
    setPC = setVar pc
    alterPC = alterVar pc

    getSP = getVar sp
    setSP = setVar sp
    alterSP = alterVar sp

    getStatus = getVar status
    setStatus = setVar status
    alterStatus = alterVar_ status

    getFlagC = getFlagBit 0
    getFlagZ = getFlagBit 1
    getFlagI = getFlagBit 2
    getFlagD = getFlagBit 3
    getFlagB = getFlagBit 4
    getFlagQ = getFlagBit 5
    getFlagV = getFlagBit 6
    getFlagN = getFlagBit 7

    setFlagC = setFlagBit 0
    setFlagZ = setFlagBit 1
    setFlagI = setFlagBit 2
    setFlagD = setFlagBit 3
    setFlagB = setFlagBit 4
    setFlagQ = setFlagBit 5
    setFlagV = setFlagBit 6
    setFlagN = setFlagBit 7

    readMemory addr
      | addr < 0x2000 = readArr lowMem (addr `mod` 0x800)
      | addr < 0x4000 = let addr' = 0x2000 + (addr `mod` 8)
                        in do setAction $ Read addr'
                              readArr ppuMem addr'
      | addr == 0x4016 = return 0
      | addr == 0x4017 = return 0
      | otherwise = readArr uppMem addr

    writeMemory addr op
      | addr < 0x2000 = writeArr lowMem (addr `mod` 0x800) op
      | addr < 0x4000 = let addr' = (0x2000 + addr `mod` 8)
                        in do setAction $ Write addr' op
                              writeArr ppuMem addr' op
      | addr == 0x4014 = do
          addr' <- (return op) <#> (readArr ppuMem 0x2003)
          ops   <- forM [addr' .. (addr' + 255)] readMemory
          setAction (DMA ops)
          writeArr uppMem 0x4014 op
      | otherwise = writeArr uppMem addr op

    alterMemory addr f = do
      res <- f <$> readMemory addr
      writeMemory addr res
      return res

    getIRQ = getVar irq
    setIRQ = setVar irq

    handlePPUAction = mapM_ handle
      where handle a = case a of Write addr op -> writeMemory addr op
                                 NMIIRQ        -> setIRQ (Just NMI)
                                 _             -> do return ()

    getCPUAction = getVar action
    setCPUAction = setVar action
