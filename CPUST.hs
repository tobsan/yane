{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CPUST
    ( CPUST
    , runCPUST
    , CPUEnv (..)
    , InputState (..)
    ) where

import Types
import CPUDefs
import CPUHelpers ((<#>))
import Control.Monad.Reader
import Control.Applicative (Applicative, (<*>), (<$>), pure)

newtype CPUST s a = CPUST
    { runCPUST :: ReaderT (CPUEnv s) (ST s) a
    } deriving ( Monad
               , MonadReader (CPUEnv s)
               , Functor
--               , Applicative
               )

instance Applicative (CPUST s) where
    pure = return
    (<*>) = ap
    

liftST :: ST s a -> CPUST s a
liftST = CPUST . lift

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

writeArr  :: (CPUEnv s -> Memory s) -> Address -> Operand -> CPUST s ()
readArr   :: (CPUEnv s -> Memory s) -> Address -> CPUST s Operand
setVar    :: (CPUEnv s -> STRef s a) -> a -> CPUST s ()
getVar    :: (CPUEnv s -> STRef s a) -> CPUST s a
alterVar  :: (CPUEnv s -> STRef s a) -> (a -> a) -> CPUST s a
alterVar_ :: (CPUEnv s -> STRef s a) -> (a -> a) -> CPUST s ()

writeArr  f addr oper = asks f >>= \r -> liftST $ writeArray r addr oper
readArr   f addr      = asks f >>= \r -> liftST $ readArray r addr
setVar    f a         = asks f >>= liftST . flip writeSTRef a
getVar    f           = asks f >>= liftST . readSTRef
alterVar  f g         = asks f >>= \r -> liftST (modifySTRef r g)
                                      >> liftST (readSTRef r)
alterVar_ f g         = asks f >>= liftST . flip modifySTRef g

-- ---------------------------------------------------------------------------
-- Functions used later on.

-- | Get bit `x` of status.
getFlagBit :: Int -> CPUST s Bool
getFlagBit x = (`testBit` x) <$> getStatus

-- | Set bit `x` of status according to bool.
setFlagBit :: Int -> Bool -> CPUST s ()
setFlagBit x b = alterStatus $ if b then (`setBit` x) else (`clearBit` x)

setAction :: Action -> CPUST s ()
setAction = setVar action

-- ---------------------------------------------------------------------------
-- | NesCPU instance for the CPUST monad

instance NesCPU (CPUST s) where
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
      | addr == 0x4016 = undefined
      | addr == 0x4017 = undefined
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
      res <- liftM f (readMemory addr)
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
