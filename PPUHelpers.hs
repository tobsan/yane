module PPUHelpers where

import PPUDefs
import Types

import Control.Monad.Reader
import Data.STRef.Lazy
import Data.Bits
import Data.Array.ST

-- | Monadic (&&)
(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
(<&&>) = liftM2 (&&)

-- | Copies the bit from location `from` to the location `to` of Bits `b`.
moveBit :: (Bits a) => a -> Int -> Int -> a
moveBit b from to = if b `testBit` from then b `setBit` to
                                        else b `clearBit` to

-- | Monadic version of `when`, if condition is fulfilled then run computation.
whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond m = do b <- cond
                  when b m

-- | Returns bit `n` of register `r`.
getBitAt :: (PPUState -> Operand) -> Int -> PPU s Bool
getBitAt r n = liftM ((`testBit` n) . r) (getVar ppuState)

-- | Memory functions
readPPUMem :: Address -> PPU s Operand
readPPUMem addr
    | addr <  0x2000 = readIt patternTables
    | addr <  0x3000 = readIt nameTables
    | addr <  0x3F00 = readPPUMem (addr - 0x1F00)
    | addr <  0x3F20 = readIt palettes
    | addr <  0x4000 = readPPUMem ((addr `mod` 0x20) + 0x3F00)
    | addr <= 0xFFFF = readPPUMem (addr `mod` 0x4000)
  where
    readIt f = asks f >>= \mem -> lift $ readArray mem addr

writePPUMem :: Address -> Operand -> PPU s ()
writePPUMem addr op
    | addr <  0x2000 = writeIt patternTables
    | addr <  0x3000 = writeIt nameTables
    | addr <  0x3F00 = writePPUMem (addr - 0x1F00) op
    | addr <  0x3F20 = writeIt palettes
    | addr <  0x4000 = writePPUMem ((addr `mod` 0x20) + 0x3F00) op
    | addr <= 0xFFFF = writePPUMem (addr `mod` 0x4000) op
  where
    writeIt f = asks f >>= \mem -> lift $ writeArray mem addr op

-- | Write data to OAM
writeOAM :: Address -> Operand -> PPU s ()
writeOAM addr op = asks ppuOAM >>= \mem -> lift $ writeArray mem addr op

-- | $2000 helpers (CONTROL1)
getPPUAddrInc :: (Num a) => PPU s a
getPPUAddrInc = do 
    b <- control1 `getBitAt` 2
    if b then return 1
         else return 32

spriteTableAddress :: PPU s Address
spriteTableAddress = do 
    b <- control1 `getBitAt` 3
    if b then return 0x0000
         else return 0x1000

bgTableAddress :: PPU s Address
bgTableAddress = do
    b <- control1 `getBitAt` 4
    if b then return 0x0000
         else return 0x1000

getSpriteSize :: (Num a) => PPU s a
getSpriteSize = do
    b <- control1 `getBitAt` 5
    if b then return 8
         else return 16

nmiOnVBlank :: PPU s Bool
nmiOnVBlank = control1 `getBitAt` 7

-- | $2001 helpers (CONTROL2)
isBGVisible :: PPU s Bool
isBGVisible = control2 `getBitAt` 3

isSpriteVisible :: PPU s Bool
isSpriteVisible = control2 `getBitAt` 4

-- | $2002 helpers (STATUS)
alterStatus :: (Operand -> Operand) -> PPU s ()
alterStatus f = alterVar ppuState (\reg -> reg { status = f (status reg) })

setSpriteOverflow, clearSpriteOverflow :: PPU s ()
setSpriteOverflow   = alterStatus (`setBit` 5)
clearSpriteOverflow = alterStatus (`clearBit` 5)

setSpriteHit, clearSpriteHit :: PPU s ()
setSpriteHit   = alterStatus (`setBit` 6)
clearSpriteHit = alterStatus (`clearBit` 6)

setVBlank, clearVBlank :: PPU s ()
setVBlank   = alterStatus (`setBit` 7)
clearVBlank = alterStatus (`clearBit` 7)

------------------------------------------------------------------------------
-- Get, set and alter variables in the environment.
getVar :: (PPURefs s -> STRef s a) -> PPU s a
getVar f = asks f >>= lift . readSTRef

setVar :: (PPURefs s -> STRef s a) -> a -> PPU s ()
setVar f a = asks f >>= lift . flip writeSTRef a

alterVar :: (PPURefs s -> STRef s a) -> (a -> a) -> PPU s ()
alterVar f a = asks f >>= lift . flip modifySTRef a

------------------------------------------------------------------------------
-- | Loopy V helpers
-- | Returns bits 0-4 of vLoopy variable.
xIndex :: PPU s Address
xIndex = liftM (.&. 0x1F) $ getVar vLoopy

-- | Returns bits 5-9 of vLoopy variable
yIndex :: PPU s Address
yIndex = liftM ((.&. 0x1F) . (`shiftL` 5)) $ getVar vLoopy

-- Handle Action
ppuHandleAction :: Action -> PPU s ()
ppuHandleAction s = case s of
    Read addr -> case addr of
        0x2002 -> do
            setVar firstWrite True
            clearVBlank
        0x2004 -> do
            alterVar sprAddr ((.&. 0xFF) . (+1))
        0x2007 -> do return () -- some funky latch stuff

    Write addr op -> case addr of
        0x2000 -> do
            alterVar ppuState $ \p -> p { control1 = op }
            alterVar tLoopy $ \t -> (t .&. 0xF3FF) .|. (((fromIntegral op) .&. 3 ) `shiftL` 10) -- check
        0x2001 -> do
            alterVar ppuState $ \p -> p { control2 = op }
        0x2002 -> do return ()
        0x2003 -> do
            setVar sprAddr (fromIntegral op) -- check
        0x2004 -> do
            writeOAM addr op
            alterVar sprAddr ((.&. 0xFF) . (+1))
        0x2005 -> do
            let op' = fromIntegral op -- check
            b <- getVar firstWrite
            if b then do
                     alterVar tLoopy $ \t -> (t .&. 0xFFE0) .|.
                                             ((op' .&. 0xF8) `shift` 3)
                     alterVar xLoopy $ \x -> (op' .&. 7)
                 else do
                     alterVar tLoopy $ \t -> (t .&. 0xFC1F) .|. 
                                             ((op' .&. 0xF8) `shift` 2)
                     alterVar tLoopy $ \t -> (t .&. 0x8FFF) .|. 
                                             ((op' .&. 7) `shift` 12)
            alterVar firstWrite not
        0x2006 -> do
            let op' = fromIntegral op -- check
            b <- getVar firstWrite
            if b then do
                     alterVar tLoopy $ \t -> (t .&. 0xFF) .|. 
                                             ((op' .&. 0x3F) `shift` 8)
                 else do
                     alterVar tLoopy $ \t -> (t .&. 0xFF00) .|. op'
                     getVar tLoopy >>= setVar vLoopy
            alterVar firstWrite not
        0x2007 -> do
            addr <- getVar vLoopy >>= readPPUMem
            writePPUMem (fromIntegral addr) op
            incLoopyV

    DMA xs -> do
        forM_ (zip [0x00..0xFF] xs) $ \(n,d) -> writeOAM n d

    NOP -> return ()

incLoopyV :: PPU s ()
incLoopyV = liftM2 (+) (getVar vLoopy) getPPUAddrInc >>= setVar vLoopy
