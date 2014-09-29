module Instr ( 
    writeMemory, readMemory, getAddress, toOper,
    fromOper, alterPC, push, pull, branchIf, mkAddr,
    compare', pullPC, pushPC, alterStatus, setZN,
    hasVSub, hasVAdd, hasCSub, hasCAdd
) where

import CPUDefs

import Data.Word
import Data.Bits
import Data.Array.Unboxed
import Control.Monad.State.Strict

--
-- This module contains various handy functions
-- used when dealing with instructions
-- Note that no implementation of instructions goes here
--

writeMemory :: Address -> Operand -> NES ()
writeMemory addr op
    | addr <  0x2000    = writeLower (addr `mod` 0x800) op
    | addr <  0x4000    = writePPU (0x2000 + addr `mod` 8) op
    | addr == 0x4014    = writeUpper 0x4014 op
    | otherwise         = writeUpper addr op

readMemory :: Address -> NES Operand
readMemory addr
    | addr <  0x2000    = readLower $ addr `mod` 0x800
    | addr <  0x4000    = readPPU   $ 0x2000 + addr `mod` 8
    | addr == 0x4014    = readUpper 0x4014
    | otherwise         = readUpper addr 

-- Write to memory below
alterMem :: (Memory -> Memory) -> NES ()
alterMem f = modify $ \cpu -> cpu { mem = f (mem cpu) }

writeLower :: Address -> Operand -> NES ()
writeLower addr op = alterMem $ \m -> m { lowMem = (lowMem m) // [(addr,op)] }

writePPU :: Address -> Operand -> NES ()
writePPU addr op = alterMem $ \m -> m { ppu = (ppu m) // [(addr,op)] }

writeUpper :: Address -> Operand -> NES ()
writeUpper addr op = alterMem $ \m -> m { uppMem = (uppMem m) // [(addr,op)] }

-- Read from memory
readLower :: Address -> NES Operand
readLower addr = gets mem >>= \mem -> return $ (lowMem mem) ! addr

readPPU :: Address -> NES Operand
readPPU addr = gets mem >>= \mem -> return $ (ppu mem) ! addr

readUpper :: Address -> NES Operand
readUpper addr = gets mem >>= \mem -> return $ (uppMem mem) ! addr

-- Translate an address mode to an address
getAddress :: AddrMode -> NES Address
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
            inx  <- gets xReg
            addr <- getAddress $ ZeroPage (Just X) op
            low  <- readMemory $ addr
            high <- readMemory $ addr+1
            return $ mkAddr low high
        otherwise   -> do
            inx  <- gets yReg
            low  <- readMemory $ transform op
            high <- readMemory $ transform (op+1)
            return $ (mkAddr low high) + (transform inx)
    Relative op     -> gets pc >>= \pc -> return $ pc + (fromIntegral op)
    Indirect addr   -> do
        low  <- readMemory addr
        if (addr .&. 0x00FF) == 0xFF -- Bug in 6502, wraps on a page boundary :D
          then readMemory (addr - 0xFF) >>= \high -> return $ mkAddr low high
          else readMemory (addr + 1) >>= \high -> return $ mkAddr low high
 where
    getIndex X = gets xReg
    getIndex Y = gets yReg

-- | Status to 8bit operand
toOper :: Status -> Operand
toOper status = foldl (\a b -> 2*a+b) 0
              . map (\s -> if (s status) then 1 else 0)
              $ [n, v, q, b, d, i, z, c]

-- Alter the status of the CPU
alterStatus :: (Status -> Status) -> NES ()
alterStatus f = modify $ \cpu -> cpu { status = f (status cpu) }

-- | 8bit operand to status record
fromOper :: Operand -> Status
fromOper oper = Status {
    c = oper .&. 0x01 /= 0,
    z = oper .&. 0x02 /= 0,
    i = oper .&. 0x04 /= 0,
    d = oper .&. 0x08 /= 0,
    b = oper .&. 0x10 /= 0,
    q = oper .&. 0x20 /= 0,
    v = oper .&. 0x40 /= 0,
    n = oper .&. 0x80 /= 0
    }

-- Handy functions to set Z and N flag
setZN :: Operand -> Status -> Status
setZN op stat = stat { z = op==0, n = op < 0 }

-- alter the PC
alterPC :: (Address -> Address) -> NES ()
alterPC f = modify $ \cpu -> cpu { pc = f (pc cpu) }

-- | Push something on the stack
push :: Operand -> NES ()
push op = do
    cpu <- get
    put $ cpu { sp = (sp cpu)-1 }
    writeMemory (baseSP + (fromIntegral $ sp cpu)) op

-- | Pull something from the stack
pull :: NES Operand
pull = do
    modify $ \cpu -> cpu { sp = (sp cpu) + 1}
    get >>= readMemory . (baseSP+) . fromIntegral . sp

-- | Update CPU program counter if @b@ is satisfied and return if succeded.
branchIf :: (Status -> Bool) -> Address -> NES Bool
branchIf b addr = do
    stat <- gets status
    if b stat
        then alterPC (const addr) >> return True
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

-- CMP, CPY, CPX
compare' :: Operand -> Operand -> Status -> Status
compare' reg mval status =
    status { c = (fromIntegral reg :: Word8) >= fromIntegral mval
           , z = reg == mval
           , n = res < 0 }
  where res = reg - mval

-- Pull the program counter from stack
pullPC :: NES Address
pullPC = liftM2 mkAddr pull pull

-- Push the program counter on stack
pushPC :: NES ()
pushPC = do
    pc <- gets pc
    push $ fromIntegral $ pc `shiftR` 8
    push $ fromIntegral $ pc

