module CPUHelpers
    ( getA, getX, getY, getPC, getSP, getCPUState, getAddress, getStatus
    , getFlagC, getFlagZ, getFlagI, getFlagD, getFlagB, getFlagV, getFlagN
    , setA, setX, setY, setPC, setSP, setStatus
    , setFlagC, setFlagZ, setFlagI, setFlagD, setFlagB, setFlagV, setFlagN
    -- does not change
    , alterA, alterX, alterY, alterSP, alterPC, alterMem, alterWithC
    , setZN
    , writeMemory
    , readMemory
    , branchIf
    , push, pushPC
    , pull, pullPC
    , hasCAdd, hasVAdd
    , hasCSub, hasVSub
    , compare'
    , mkAddr
    ) where

import Types
import CPUDefs
import Data.Array.ST
import Data.Array.MArray
import Data.STRef.Lazy
import Control.Monad.ST.Lazy
import Control.Monad.Reader
import Data.Word
import Data.Bits
-- import Data.Array.Unboxed
-- import Control.Monad.State.Strict

--
-- This module contains various handy functions
-- used when dealing with instructions
-- Note that no implementation of instructions goes here
--

writeMemory :: Address -> Operand -> CPU s ()
writeMemory addr op
    | addr <  0x2000    = do 
        mem <- asks lowMem
        lift $ writeArray mem (addr `mod` 0x800) op
    | addr <  0x4000    = do 
        mem <- asks ppuMem
        lift $ writeArray mem (0x2000 + addr `mod` 8) op
    | addr == 0x4014    = undefined --fix dma action
    | otherwise         = asks uppMem >>= \m -> lift $ writeArray m addr op

readMemory :: Address -> CPU s Operand
readMemory addr
    | addr <  0x2000 = asks lowMem >>= lift . flip readArray (addr `mod` 0x800)
    | addr <  0x4000 = let addr' = 0x2000 + addr `mod` 8
                       in asks ppuMem >>= lift . flip readArray addr'
    | otherwise      = asks uppMem >>= lift . flip readArray addr

-- Translate an address mode to an address
getAddress :: AddrMode -> CPU s Address
getAddress mode = case mode of
    ZeroPage offset addr -> case offset of
        Just ix -> do
            inx <- getIndex ix
            let addr' = fromIntegral addr   :: Word8
            let inx'  = fromIntegral inx    :: Word8
            return $ fromIntegral $ addr+inx
        otherwise -> return $ transform addr
    Absolute offset addr -> case offset of
        Just ix     -> getIndex ix >>= \inx -> return $ addr + (transform inx)
        otherwise   -> return addr
    Indexed when op -> case when of
        Before -> do
            inx  <- getX
            addr <- getAddress $ ZeroPage (Just X) op
            low  <- readMemory $ addr
            high <- readMemory $ addr+1
            return $ mkAddr low high
        otherwise   -> do
            inx  <- getY
            low  <- readMemory $ transform op
            high <- readMemory $ transform (op+1)
            return $ (mkAddr low high) + (transform inx)
    Relative op     -> do
            pc <- getPC
            return $ pc + (fromIntegral op)
    Indirect addr   -> do
        low  <- readMemory addr
        -- | Bug in 6502, wraps on a page boundary :D
        let offset = if (addr .&. 0x00FF) == 0xFF
                        then (addr - 0xFF)
                        else (addr + 1)
        readMemory offset >>= \high -> return $ mkAddr low high

 where
    getIndex X = getCPUState >>= return . xReg
    getIndex Y = getCPUState >>= return . yReg

-- | Push something on the stack
push :: Operand -> CPU s ()
push op = do res <- getSP
             setSP $ res-1
             writeMemory (baseSP + (fromIntegral res)) op

-- | Pull something from the stack
pull :: CPU s Operand
pull = alterSP (+1) >>= readMemory . (baseSP+) . fromIntegral

-- | Update CPU program counter if @b@ is satisfied and return if succeded.
branchIf :: Address -> Bool -> CPU s Bool
branchIf addr b = if b then alterPC (const addr) >> return True
                       else return False

-- build a 16-bit address from low and high components
mkAddr :: Operand -> Operand -> Address
mkAddr low high = ((transform high) `shift` 8) + (transform low)

-- cast an Int8 to a Word16 without promotion of sign
transform :: Operand -> Address
transform op = (fromIntegral op) .&. 0x00FF

-- checks if we have overflow in an addition
hasVAdd :: Operand -> Operand -> Operand -> Bool
hasVAdd op1 op2 res 
    | op1 >= 0 && op2 >= 0 && res < 0   = True
    | op1 < 0  && op2 < 0  && res >= 0  = True
    | otherwise                         = False

-- checks if we have overflow in a subtraction
hasVSub :: Operand -> Operand -> Operand -> Bool
hasVSub op1 op2 res
    | op1 >= 0 && op2 <  0 && res <  0 = True
    | op1 <  0 && op2 >= 0 && res >= 0 = True
    | otherwise                        = False

-- checks if we have carry in an addition
hasCAdd :: Operand -> Operand -> Bool
hasCAdd op1 op2 = res > 0xFF
    where res = transform op1 + transform op2

-- checks if we have carry in an subtraction
hasCSub :: Operand -> Operand -> Bool
hasCSub op1 op2 = res < 0x100
    where res = transform op1 - transform op2

compare' :: Operand -> Operand -> CPU s ()
compare' mval reg = do
    setFlagC $ (fromIntegral reg :: Word8) >= fromIntegral mval
    setFlagZ $ reg == mval
    setFlagN $ res < 0
  where res = reg - mval

------------------------------------------------------------------------------
-- Functions used by the primitive functions.

getCPUState :: CPU s CPUState
getCPUState = asks cpuState >>= lift . readSTRef

putCPUState :: CPUState -> CPU s ()
putCPUState c = asks cpuState >>= lift . flip writeSTRef c 

modCPUState :: (CPUState -> CPUState) -> CPU s ()
modCPUState f = asks cpuState >>= lift . flip modifySTRef f

alterStatus :: (Operand -> Operand) -> CPU s ()
alterStatus f = getStatus >>= setStatus . f

getStatus :: CPU s Operand
getStatus = liftM status getCPUState

setStatus :: Operand -> CPU s ()
setStatus s = modCPUState $ \c -> c { status = s }

-- Primitive functions.
------------------------------------------------------------------------------
-- Get register.
getA, getX, getY :: CPU s Operand
getA = liftM aReg getCPUState
getX = liftM xReg getCPUState
getY = liftM yReg getCPUState

-- Get program counter.
getPC :: CPU s Address
getPC = liftM pc getCPUState

-- Get stack pointer.
getSP :: CPU s Word8
getSP = liftM sp getCPUState

-- Get flag.
getFlagC, getFlagZ, getFlagI, getFlagD, getFlagB, getFlagV, getFlagN 
    :: CPU s Bool
getFlagC = getFlagBit 0
getFlagZ = getFlagBit 1
getFlagI = getFlagBit 2
getFlagD = getFlagBit 3
getFlagB = getFlagBit 4
getFlagQ = getFlagBit 5
getFlagV = getFlagBit 6
getFlagN = getFlagBit 7

-- auxiliary
getFlagBit :: Int -> CPU s Bool
getFlagBit x = liftM ((`testBit` x) . status) getCPUState

------------------------------------------------------------------------------
-- Set register.
setA, setX, setY :: Operand -> CPU s ()
setA op = modCPUState $ \cpu -> cpu { aReg = op }
setX op = modCPUState $ \cpu -> cpu { xReg = op }
setY op = modCPUState $ \cpu -> cpu { yReg = op }

-- Set program counter.
setPC :: Address -> CPU s ()
setPC addr = modCPUState $ \cpu -> cpu { pc = addr }

-- Set stack pointer.
setSP :: Word8 -> CPU s ()
setSP addr = modCPUState $ \cpu -> cpu { sp = addr }

-- Set flag.
setFlagC, setFlagZ, setFlagI, setFlagD, setFlagB, setFlagV, setFlagN, setFlagQ
    :: Bool -> CPU s ()
setFlagC q = setFlag q 0
setFlagZ q = setFlag q 1 
setFlagI q = setFlag q 2
setFlagD q = setFlag q 3
setFlagB q = setFlag q 4
setFlagQ q = setFlag q 5
setFlagV q = setFlag q 6
setFlagN q = setFlag q 7

setFlag :: Bool -> Int -> CPU s ()
setFlag q x = if q then alterStatus (`setBit` x) else alterStatus (`clearBit` x)

------------------------------------------------------------------------------
-- Functions based on the primitives.
-- Alter registers and returns result
alterA, alterX, alterY :: (Operand -> Operand) -> CPU s Operand
alterA f = getA >>= setA . f >> getA
alterX f = getX >>= setX . f >> getX
alterY f = getY >>= setY . f >> getY

alterSP :: (Word8 -> Word8) -> CPU s Word8
alterSP f = getSP >>= setSP . f >> getSP

alterPC :: (Address -> Address) -> CPU s ()
alterPC f = getPC >>= setPC . f

alterMem :: Address -> (Operand -> Operand) -> CPU s Operand
alterMem addr f = do m <- readMemory addr
                     let res = f m in writeMemory addr res >> return res

-- Set Z and N flag.
setZN :: Operand -> CPU s ()
setZN op = setFlagZ (op==0) >> setFlagN (op < 0)

-- Pull the program counter from stack
pullPC :: CPU s Address
pullPC = liftM2 mkAddr pull pull

-- Push the program counter on stack
pushPC :: CPU s ()
pushPC = do res <- getPC
            push $ fromIntegral $ res `shiftR` 8
            push $ fromIntegral $ res

-- Alters and set C flag if bit 7 is set. 
-- *** FIXME ***  okay, spagettikod men bara för att testa.
alterWithC :: ((Operand -> Operand) -> CPU s Operand)
           -> (Operand -> Operand)
           -> CPU s Operand
alterWithC m f = do m id >>= setFlagC . (`testBit` 7) >> m f
