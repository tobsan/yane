{-# LANGUAGE BangPatterns #-}
module PPUHelpers where

import PPUDefs
import Types

import Control.Monad.Reader
import Data.Array.ST
import Data.List

-- import Debug.Trace
import Data.Array.Unboxed

import Data.Bits (bit)

import Numeric (showHex)

-- | Monadic (&&)
(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
(<&&>) = liftM2 (&&)

-- | Copies the bit from location `from` to the location `to` of Bits `b`.
moveBit :: (Num a, Bits a) => a -> Int -> Int -> a
moveBit b from to = if b `testBit` from then bit to else 0

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
    | addr <  0x2000 = readIt patternTables addr
    | addr <  0x3000 = readIt nameTables (verticalMirror addr)
    | addr <  0x3F00 = readPPUMem (addr - 0x1F00)
    | addr <  0x3F20 = readIt palettes addr
    | addr <  0x4000 = readPPUMem ((addr `mod` 0x20) + 0x3F00)
    | addr <= 0xFFFF = readPPUMem (addr `mod` 0x4000)
    | otherwise = error "dummy!"
  where
    readIt f addr' = asks f >>= \mem -> lift $ readArray mem addr'

writePPUMem :: Address -> Operand -> PPU s ()
writePPUMem addr op
    | addr <  0x2000 = writeIt patternTables addr
    | addr <  0x3000 = writeIt nameTables (verticalMirror addr)
    | addr <  0x3F00 = writePPUMem (addr - 0x1F00) op
    | addr <  0x3F20 = writeIt palettes addr
    | addr <  0x4000 = writePPUMem ((addr `mod` 0x20) + 0x3F00) op
    | addr <= 0xFFFF = writePPUMem (addr `mod` 0x4000) op
    | otherwise = error "dummy"
  where
    writeIt f addr' = asks f >>= \mem -> lift $ writeArray mem addr' op

verticalMirror :: Address -> Address
verticalMirror addr
    | addr < 0x2800 = addr
    | addr < 0x3000 = addr - 0x800
    | otherwise = error "dummy"

horizontalMirror :: Address -> Address
horizontalMirror addr
    | addr < 0x2400 = addr
    | addr < 0x2800 = addr - 0x400
    | addr < 0x2C00 = addr
    | addr < 0x3000 = addr - 0x400
    | otherwise = error "dummy"

-- | Write data to OAM
writeOAM :: Address -> Operand -> PPU s ()
writeOAM addr op = asks ppuOAM >>= \mem -> lift $ writeArray mem addr op

readOAM :: Address -> PPU s Operand
readOAM addr = asks ppuOAM >>= lift . flip readArray addr

-- | $2000 helpers (CONTROL1)
getPPUAddrInc :: (Num a) => PPU s a
getPPUAddrInc = do 
    b <- control1 `getBitAt` 2
    if b then return 32
         else return 1

bgTableAddress :: PPU s Address
bgTableAddress = do
    b <- control1 `getBitAt` 4
    if b then return 0x1000
         else return 0x0000

spriteTableAddress :: PPU s Address
spriteTableAddress = do 
    b <- control1 `getBitAt` 3
    if b then return 0x1000
         else return 0x0000

getSpriteSize :: (Num a) => PPU s a
getSpriteSize = do
    b <- control1 `getBitAt` 5
    if b then return 16
         else return 8

nmiOnVBlank :: PPU s Bool
nmiOnVBlank = control1 `getBitAt` 7

-- | $2001 helpers (CONTROL2)
isBGVisible :: PPU s Bool
isBGVisible = control2 `getBitAt` 3

isSpriteVisible :: PPU s Bool
isSpriteVisible = control2 `getBitAt` 4

-- | $2002 helpers (STATUS)
alterStatus :: (Operand -> Operand) -> PPU s ()
alterStatus f = do
--    alterVar ppuState (\reg -> reg { status = f (status reg) })
--    getVar ppuState >>= addPPUAction . Write 0x2002 . status
    stat <- liftM status $ getVar ppuState
    when (stat /= (f stat)) $ do
        alterVar ppuState (\reg -> reg { status = f (status reg) })
        getVar ppuState >>= addPPUAction . Write 0x2002 . status


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
getVar :: (PPUEnv s -> STRef s a) -> PPU s a
getVar f = asks f >>= lift . readSTRef

setVar :: (PPUEnv s -> STRef s a) -> a -> PPU s ()
setVar f a = asks f >>= lift . flip writeSTRef a

alterVar :: (PPUEnv s -> STRef s a) -> (a -> a) -> PPU s ()
alterVar f a = asks f >>= lift . flip modifySTRef a

------------------------------------------------------------------------------
-- | Loopy V helpers
-- | Returns bits 0-4 of vLoopy variable.
xIndex :: PPU s Address
xIndex = liftM (.&. 0x1F) $ getVar vLoopy

-- | Returns bits 5-9 of vLoopy variable
yIndex :: PPU s Address
yIndex = liftM ((`shiftR` 5) . (.&. 0x3E0)) $ getVar vLoopy
------------------------------------------------------------------------------
-- Actions
addPPUAction :: Action -> PPU s ()
addPPUAction = alterVar ppuActions . (:)

-- only returns the latest updates.
getPPUActions :: PPU s [Action]
getPPUActions = liftM (map head . group . sort) $ getVar ppuActions

-- Handle Action
ppuDecodeAction :: Action -> PPU s ()
ppuDecodeAction s = case s of
    Read addr -> case addr of
        0x2002 -> do -- PPUSTATUS
            setVar firstWrite True
            clearVBlank            -- *action*
        0x2004 -> do -- OAMDATA
            return ()
-- should not increase sprAddr
--            alterVar sprAddr ((.&. 0xFF) . (+1))
-- should not be returned since it doesn't change addr (no new data)
--            getVar sprAddr >>= readOAM >>= addAction . Write 0x2004
        0x2007 -> do -- PPUDATA
            getVar vLoopy >>= readPPUMem >>= addPPUAction . Write 0x2007
            incLoopyV
        _ -> return ()

    Write addr op -> case addr of
        0x2000 -> do -- CONTROL1
            alterVar ppuState $ \p -> p { control1 = op }
            alterVar tLoopy $
                \t -> (t .&. 0xF3FF) .|.  (((transform op) .&. 3 ) `shiftL` 10)
        0x2001 -> do -- CONTROL2
            alterVar ppuState $ \p -> p { control2 = op }
        0x2002 -> do -- PPUSTATUS
            return () -- should never be written to.
        0x2003 -> do -- OAMADDR
            setVar sprAddr (transform op) -- check
        0x2004 -> do -- OAMDATA
            writeOAM addr op
            alterVar sprAddr ((.&. 0xFF) . (+1))
            getVar sprAddr >>= readOAM >>= addPPUAction . Write 0x2004
        0x2005 -> do -- PPUSCROLL
            let op' = transform op
            b <- getVar firstWrite
            if b then do
                     alterVar tLoopy $ \t ->
                        (t .&. 0xFFE0) .|. ((op' .&. 0xF8) `shiftR` 3)
                     setVar xLoopy $ (op' .&. 7)
                 else do
                     alterVar tLoopy $ \t ->
                        (t .&. 0xFC1F) .|. ((op' .&. 0xF8) `shiftL` 2)
                     alterVar tLoopy $ \t ->
                        (t .&. 0x8FFF) .|. ((op' .&. 7) `shiftL` 12)
            alterVar firstWrite not
        0x2006 -> do -- PPUADDR
            let op' = (transform op)
            b <-  (getVar firstWrite)
            if b then do
                     alterVar tLoopy $ \t ->
                        (t .&. 0xFF) .|. ((op' .&. 0x3F) `shiftL` 8)
                 else do
                     alterVar tLoopy $ \t -> (t .&. 0xFF00) .|. op'
                     getVar tLoopy >>= setVar vLoopy
            alterVar firstWrite not
        0x2007 -> do -- PPUDATA
            v <- getVar vLoopy
            writePPUMem v op
            incLoopyV
        _ -> error "dummy"

    DMA xs -> do
        forM_ (zip [0x00..0xFF] xs) $ \(n,d) -> writeOAM n d

    NOP -> return ()
    _ -> error "dummy"

incLoopyV :: PPU s ()
incLoopyV = getPPUAddrInc >>= alterVar vLoopy . (+)

--------------------------------------------------------------------------------------------------------

makeItRGB :: Operand -> Pixel
makeItRGB = (colors !) . (`mod` 63)

colors :: UArray Operand Pixel
colors = listArray (0,63)
    [ 0x808080, 0x003DA6, 0x0012B0, 0x440096, 0xA1005E, 0xC70028, 0xBA0600, 0x8C1700
    , 0x5C2F00, 0x104500, 0x054A00, 0x00472E, 0x004166, 0x000000, 0x050505, 0x050505
    , 0xC7C7C7, 0x0077FF, 0x2155FF, 0x8237FA, 0xEB2FB5, 0xFF2950, 0xFF2000, 0xD63200
    , 0xC46200, 0x358000, 0x058F00, 0x008A55, 0x0099CC, 0x212121, 0x090909, 0x090909
    , 0xFFFFFF, 0x0FD7FF, 0x69A2FF, 0xD480FF, 0xFF45F3, 0xFF618B, 0xFF8833, 0xFF9C12
    , 0xFABC20, 0x9FE30E, 0x2BF035, 0x0CF0A4, 0x05FBFF, 0x5E5E5E, 0x0D0D0D, 0x0D0D0D
    , 0xFFFFFF, 0xA6FCFF, 0xB3ECFF, 0xDAABEB, 0xFFA8F9, 0xFFABB3, 0xFFD2B0, 0xFFEFA6
    , 0xFFF79C, 0xD7E895, 0xA6EDAF, 0xA2F2DA, 0x99FFFC, 0xDDDDDD, 0x111111, 0x111111
    ]

inHex :: Integral m => m -> String
inHex m = "0x" ++ showHex (fromIntegral m :: Word16) ""
