{-# LANGUAGE BangPatterns #-}
module PPU where

import Types
import PPUHelpers
import Control.Monad
import Debug.Trace

-- Runs PPU and produces `n` pixels. (1 pixel per cc)
runPPU :: Int -> PPU s [Pixel]
runPPU 0  = return []
runPPU cc = do
    setVar ppuActions [] -- reset actions
    !scc <- (getVar scanlineCycle)
    (scc', pxs) <- runPPU' cc scc
    setVar scanlineCycle scc'
    return pxs
  where
    runPPU' 0  !scc = return (scc, [])
    runPPU' !cc scc
        | scc == 341 = do 
            runScanline
            !pxs <- getVar pixels
            (scc', xs) <- runPPU' (cc - 1) 0
            return (scc', (reverse pxs)++xs)
        | otherwise = runPPU' (cc - 1) (scc+1)

runScanline :: PPU s ()
runScanline = do
    sc  <- getVar scanline
    (setVar pixels []) -- reset pixels
    case () of () | sc <= 19  -> do -- vBlank
                      alterVar scanline (+1)
                  | sc == 20  -> do -- dummy
                      whenM (isBGVisible <&&> isSpriteVisible)
                          (getVar tLoopy >>= setVar vLoopy)
                      clearSpriteHit
                      clearVBlank
                      alterVar scanline (+1)
                  | sc <= 260 -> do -- real deal
                      whenM (isBGVisible <&&> isSpriteVisible) genScanline
                      when (sc == 51) (setSpriteHit)
                      alterVar scanline (+1)
                  | sc == 263 -> do -- clean up
                      setVBlank
                      setVar scanline 0
                      whenM nmiOnVBlank $ addPPUAction NMIIRQ
                  | otherwise -> do -- life saver
                      alterVar scanline (+1)

genScanline :: PPU s ()
genScanline = do
    t <- liftM (.&. 0x41F)  $ getVar tLoopy
    alterVar vLoopy $ \v -> (v .&. 0xFBE0) .|. t

    whenM isBGVisible genBackground
--    whenM isSpriteVisible $ do
--        sl <- liftM (subtract 20) $ getVar scanline
--        genSprites 0 1 sl

    alterVar vLoopy $ \v1 ->
        if (v1 .&. 0x7000) == 0x7000
            then let v2 = v1 .&. 0x8FFF
                 in if (v2 .&. 0x3E0) == 0x3A0
                       then (v2 `xor` 0x800) .&. 0xFC1F
                       else if (v2 .&. 0x3E0) == 0x3E0
                                then (v2 .&. 0xFC1F)
                                else v2 + 0x20
            else v1 + 0x1000

genSprites :: Address -> Int -> Int -> PPU s ()
genSprites oamIndex numSprites sl = do
    y            <- readOAM  (oamIndex * 4) >>= return . (+1)
    patternIndex <- readOAM $ oamIndex * 4 + 1
    spriteAttrib <- readOAM $ oamIndex * 4 + 2
    x            <- readOAM $ oamIndex * 4 + 3
    height       <- getSpriteSize
 
    let vFlip = spriteAttrib `testBit` 7
    let hFlip = spriteAttrib `testBit` 6
    let prio  = spriteAttrib `testBit` 5
    let colorMSB = (spriteAttrib `shiftL` 2) .&. 0xC

    let line = (fromIntegral sl) - (transform y)
    unless (line < 0 || line >= height) $ do
        let lineRev = if vFlip then height - 1 - line else line

        when (numSprites > 8) setSpriteOverflow

        patternAddr <- do
            sprAddr <- spriteTableAddress
            return $ (transform patternIndex) * 0x10 + line + sprAddr
        patternLSB <- readOAM patternAddr
        patternMSB <- readOAM $ patternAddr + 8

        -- todo sprite size 16.
        forM_ [7,6..0] $ \i -> genSpriteRow i x
    return ()

genSpriteRow :: Operand -> Operand -> PPU s ()
genSpriteRow idx x = do return ()

genBackground :: PPU s ()
genBackground = do
    v <- getVar vLoopy
    x <- xIndex
    y <- yIndex

    ntAddr <- return $ 0x2000 + (v .&. 0xFFF)

    atAddr <- let y' = (y .&. 0xFFC) `shiftL` 1
                  x' = x `shiftR` 2
                  attribOffset = 0x3C0
              in return $ 0x2000 + attribOffset + y' + x' + (v .&. 0xC00)

    calcPatterns 32 0 ntAddr atAddr x y

-- | 
calcPatterns :: Int -> Operand -> Address -> Address -> Address -> Address -> PPU s ()
calcPatterns 0 _ _ _ _ _ = return ()
calcPatterns i attrib ntAddr atAddr x y = do
    -- Calculate attribute
    attrib' <- if (i == 32 || (x .&. 0x1) == 0)
                  then calcAttribute atAddr x y
                  else return attrib

    -- Calculate pattern
    patternAddr <- do
        bgAddr   <- bgTableAddress
        yOffset  <- liftM ((`shiftR` 12) . (.&. 0x7000)) $ getVar vLoopy
        tileBase <- liftM ((`shiftL` 4) . transform)   $ readPPUMem ntAddr
        return $ bgAddr + yOffset + tileBase

    patternLSB <- readPPUMem $ patternAddr
    patternMSB <- readPPUMem $ patternAddr + 8

    -- Genererate a row in a tile.
    forM_ [7,6..0] $ \idx -> do
        let pattern = (moveBit patternMSB idx 1) .|. (moveBit patternLSB idx 0)
            colorAddr | pattern == 0 = 0x3F10
                      | otherwise    = 0x3F00 + ((transform attrib')
                                             .|. (transform pattern))
        color <- liftM makeItRGB (readPPUMem colorAddr)
        alterVar pixels (color:) -- adds pixel

    -- Update variables.
    let !ntAddr' = ntAddr + 1
    let !atAddr' = atAddr + 1
    let !x' = x + 1
    let !i' = i - 1

    if (x' .&. 0x3) == 0
        then if (x' .&. 0x1F) == 0
                then calcPatterns i'
                                  attrib'
                                  ((ntAddr' `xor` 0x400) - 32)
                                  ((atAddr `xor` 0x400) - 8 + 1)
                                  (x' - 32)
                                  y
                else calcPatterns i' attrib' ntAddr' atAddr' x' y
        else calcPatterns i' attrib' ntAddr' atAddr x' y

calcAttribute :: Address -> Address -> Address -> PPU s Operand
calcAttribute atAddr idX idY = liftM ((.&. 0xF) . bitMask) (readPPUMem atAddr)
  where
    bY = idY `testBit` 1
    bX = idX `testBit` 1
    bitMask | bY     && bX     = (`shiftR` 4) . (.&. 0xC0)    -- 1100 0000
            | bY     && not bX = (`shiftR` 2) . (.&. 0x30)    -- 0011 0000
            | not bY && not bX = (`shiftL` 2) . (.&. 0x3)     -- 0000 1100
            | not bY && bX     = (.&. 0xC)                    -- 0000 0011
