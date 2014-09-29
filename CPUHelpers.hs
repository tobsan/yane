module CPUHelpers
    ( getA, getX, getY, getPC, getSP, getAddress, getStatus
    , getFlagC, getFlagZ, getFlagI, getFlagD, getFlagB, getFlagV, getFlagN
    , setA, setX, setY, setPC, setSP, setStatus
    , setFlagC, setFlagZ, setFlagI, setFlagD, setFlagB, setFlagV, setFlagN
    , readPlayer, writePlayer
    -- does not change
    , alterA, alterX, alterY, alterSP, alterPC, alterMem, alterSetC
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
    , handleIRQ
    , cpuDecodeActions
    , getCPUAction
    , setAction
    ) where

import Types
import CPUDefs
import Data.Array.ST
import Control.Monad.Reader
import Data.Maybe (isJust)

-- import Data.Array.Unboxed
-- import Control.Monad.State.Strict

--
-- This module contains various handy functions
-- used when dealing with instructions
-- Note that no implementation of instructions goes here
--

getCPUAction :: CPU s Action
getCPUAction = getVar action
 -- -getRef action >>= lift . readSTRef -- >>= \r -> trace (show r) (return r) --ask >>= return . action

setAction :: Action -> CPU s ()
setAction = setVar action -- getRef action >>= lift . flip writeSTRef act

cpuDecodeActions :: [Action] -> CPU s ()
cpuDecodeActions = mapM_ handle
    where
        handle a = case a of
            Write addr op -> writeMemory addr op
            NMIIRQ        -> setVar irq (Just NMI) -- getRef irq >>= lift . flip writeSTRef (Just NMI)
            _             -> do return ()

handleIRQ :: CPU s ()
handleIRQ = do
    i <- getVar irq
    case i of
        Nothing  -> do
            return ()
        Just Normal -> do
            pushPC
            getStatus >>= push
            setFlagI True
            setFlagB False
            lowAddr  <- readMemory 0xFFFE
            highAddr <- readMemory 0xFFFF
            setPC $ mkAddr lowAddr highAddr
        Just NMI -> do
            pushPC
            getStatus >>= push
            lowAddr  <- readMemory 0xFFFA
            highAddr <- readMemory 0xFFFB
            setPC $ mkAddr lowAddr highAddr
        Just Reset -> do
            lowAddr  <- readMemory 0xFFFC
            highAddr <- readMemory 0xFFFD
            setPC $ mkAddr lowAddr highAddr
    when (isJust i) $ setVar irq Nothing

writeMemory :: Address -> Operand -> CPU s ()
writeMemory addr op
    | addr <  0x2000    = do
        mem <- asks lowMem
        lift $ writeArray mem (addr `mod` 0x800) op
    | addr <  0x4000    = do
      let addr' = (0x2000 + addr `mod` 8)
      mem <- asks ppuMem
      setAction (Write addr' op)
      lift $ writeArray mem addr' op
    -- | DMA (maybe * 0x100 instead of readMem)?
    | addr == 0x4014    = do
      mem <- asks uppMem
      ppuMem <- asks ppuMem
      lsb <- lift $ readArray ppuMem 0x2003           -- readMemory would add Read Action
      let addr' = mkAddr op lsb
      ops <- forM [addr'..addr'+255] readMemory
      setAction (DMA ops)
      lift $ writeArray mem 0x4014 op
    | otherwise         = asks uppMem >>= \m -> lift $ writeArray m addr op

readMemory :: Address -> CPU s Operand
readMemory addr
    | addr <  0x2000 = asks lowMem >>= lift . flip readArray (addr `mod` 0x800)
    | addr <  0x4000 = do
      let addr' = 0x2000 + addr `mod` 8
      setAction (Read addr')
      asks ppuMem >>= lift . flip readArray addr'
    | addr == 0x4016 = readPlayer player1 True
    | addr == 0x4017 = readPlayer player2 False
    | otherwise      = do
        asks uppMem >>= lift . flip readArray addr

-- reads the input from a player controller
readPlayer :: (SysEnv s -> STRef s InputState) -> Bool -> CPU s Operand
readPlayer f b = do
    player <- getRef f
    pstate <- lift $ readSTRef player
    -- each read cycle is 24 reads
    lift $ modifySTRef player $ \st -> st { readNum = ((readNum st) + 1 `mod` 24) }
    return $ readPlayer' pstate b
 where
    readPlayer' :: InputState -> Bool -> Operand
    readPlayer' (InputState reg num) b
        | num < 8  = if reg `testBit` num then 0x1 else 0x0
        | num == 17 = if b then 0x0 else 0x1
        | num == 18 = if b then 0x1 else 0x0
        | num == 19 = 0xB
        | otherwise = 0x0

-- set the value for a input controller
writePlayer :: (SysEnv s -> STRef s InputState) -> (Operand -> Operand) -> CPU s ()
writePlayer f fop = alterVar f $ \st -> st { register = fop (register st) }

--    player <- getRef f
--    lift $ modifySTRef player $ \st -> st { register = fop (register st) }

-- Translate an address mode to an address
getAddress :: AddrMode -> CPU s Address
getAddress mode = case mode of
    ZeroPage offset addr -> case offset of
        Just ix -> do
            inx <- getIndex ix
            let addr' = fromIntegral addr   :: Word8
            let inx'  = fromIntegral inx    :: Word8
            return $ fromIntegral $ addr'+inx'
        _      -> return $ transform addr
    Absolute offset addr -> case offset of
        Just ix     -> getIndex ix >>= \inx -> return $ addr + (transform inx)
        _           -> return addr
    Indexed when op -> case when of
        Before -> do
            addr <- getAddress $ ZeroPage (Just X) op
            low  <- readMemory $ addr
            high <- readMemory $ addr+1
            return $ mkAddr low high
        _      -> do
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
    getIndex X = getX
    getIndex Y = getY

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
branchIf addr b = if b then setPC addr >> return True
                       else return False

-- build a 16-bit address from low and high components
mkAddr :: Operand -> Operand -> Address
mkAddr low high = ((transform high) `shiftL` 8) .|. (transform low)

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
    setFlagC $ transform reg >= transform mval
    setFlagZ $ reg == mval
    setFlagN $ ((fromIntegral reg :: Int16) - fromIntegral mval) < 0

------------------------------------------------------------------------------
-- Functions used by the primitive functions.

--getCPUState :: CPU s (CPUState s)
--getCPUState = asks cpuState >>= lift . readSTRef

--getRef :: (CPUState s -> STRef s a) -> CPU s (STRef s a)
--getRef f = liftM f getCPUState

setVar :: (SysEnv s -> STRef s a) -> a ->  CPU s ()
setVar f a = asks f >>= lift . flip writeSTRef a

getVar :: (SysEnv s -> STRef s a) -> CPU s a
getVar f = asks f >>= lift . readSTRef

alterVar :: (SysEnv s -> STRef s a) -> (a -> a) -> CPU s ()
alterVar f a = asks f >>= lift . flip modifySTRef a

getRef :: (SysEnv s -> STRef s a) -> CPU s (STRef s a)
getRef f = asks f

alterStatus :: (Operand -> Operand) -> CPU s ()
alterStatus = alterVar status

getStatus :: CPU s Operand
getStatus =  getVar status

setStatus :: Operand -> CPU s ()
setStatus = setVar status

-- Primitive functions.
------------------------------------------------------------------------------
-- Get register
getA, getX, getY :: CPU s Operand
getA = getVar aReg
getX = getVar xReg
getY = getVar yReg

-- Get program counter.
getPC :: CPU s Address
getPC = getVar pc

-- Get stack pointer.
getSP :: CPU s Word8
getSP = getVar sp

-----------------------------------------------------------------------------
-- Set register.
setA, setX, setY :: Operand -> CPU s ()
setA = setVar aReg
setX = setVar xReg
setY = setVar yReg

-- Set program counter.
setPC :: Address -> CPU s ()
setPC = setVar pc

-- Set stack pointer.
setSP :: Word8 -> CPU s ()
setSP = setVar sp

-----------------------------------------------------------------------------
-- | Flags!

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

setFlagC, setFlagZ, setFlagI, setFlagD, setFlagB, setFlagV, setFlagN, setFlagQ
    :: Bool -> CPU s ()
setFlagC = setFlag 0
setFlagZ = setFlag 1
setFlagI = setFlag 2
setFlagD = setFlag 3
setFlagB = setFlag 4
setFlagQ = setFlag 5
setFlagV = setFlag 6
setFlagN = setFlag 7

-- auxiliary
getFlagBit :: Int -> CPU s Bool
getFlagBit x = liftM (`testBit` x) getStatus

setFlag :: Int -> Bool -> CPU s ()
setFlag x q = if q then alterStatus (`setBit` x) else alterStatus (`clearBit` x)

------------------------------------------------------------------------------
-- Alter registers and returns result

alterReg :: (SysEnv s -> STRef s a) -> (a -> a) -> CPU s a
alterReg f g = do
    val <- getVar f
    let res = g val
    setVar f res
    return res

alterA, alterX, alterY :: (Operand -> Operand) -> CPU s Operand
alterA = alterReg aReg
alterX = alterReg xReg
alterY = alterReg yReg

alterSP :: (Word8 -> Word8) -> CPU s Word8
alterSP = alterReg sp

alterPC :: (Address -> Address) -> CPU s Address
alterPC = alterReg pc

alterMem :: Address -> (Operand -> Operand) -> CPU s Operand
alterMem addr f = do 
    m <- readMemory addr
    let res = f m
    writeMemory addr res
    return res

-- Set Z and N flag.
setZN :: Operand -> CPU s ()
setZN op = alterStatus (z . n)
  where
    z = if op == 0 then (`setBit` 1) else (`clearBit` 1)
    n = if op < 0 then (`setBit` 7) else (`clearBit` 7)

-- Pull the program counter from stack
pullPC :: CPU s Address
pullPC = liftM2 mkAddr pull pull

-- Push the program counter on stack
pushPC :: CPU s ()
pushPC = do res <- getPC
            push $ fromIntegral $ res `shiftR` 8
            push $ fromIntegral $ res

-- Set C flag to current value of `m` at bit `b` and then alters `m` with `f`.
alterSetC  :: Int
           -> ((Operand -> Operand) -> CPU s Operand)
           -> (Operand -> Operand)
           -> CPU s Operand
alterSetC b m f = do
    val <- m id
    setFlagC $ (val `testBit` b)
    m f
