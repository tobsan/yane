module PPU where

import Types
import PPUDefs
import PPUHelpers

import Data.Bits
import Control.Monad

-- FIXME
getFirstPixelInBuffer = undefined
signalNmiInterrupt = undefined
makeItRGB = undefined

-- Runs PPU and produces `n` pixels. (1 pixel per cc)
runPPU :: Int -> PPU s [Pixel]
runPPU 0  = return []
runPPU cc = do
    scc <- getVar scanlineCycle
    case () of () | scc == 341 -> do setVar scanlineCycle 0
                                     runScanline
                                     runPPU (cc-1)
                  | otherwise -> do alterVar scanlineCycle $ subtract 1
                                    mb <- getFirstPixelInBuffer
                                    case mb of
                                        Nothing -> runPPU (cc-1)
                                        Just x  -> do xs <- runPPU (cc-1)
                                                      return (x:xs)

runScanline :: PPU s ()
runScanline = do
    sc  <- getVar scanline
    case () of () | sc <= 19  -> do -- vBlank
                      alterVar scanline (+1)
                  | sc == 20  -> do -- dummy
                      whenM (isBGVisible <&&> isSpriteVisible)
                          (getVar tLoopy >>= setVar vLoopy)
                      clearSpriteHit
                      clearVBlank
                      alterVar scanline (+1)
                  | sc <= 260 -> do -- real deal
                      whenM (isBGVisible <&&> isSpriteVisible) genSLPixel
                      alterVar scanline (+1)
                  | sc == 263 -> do -- clean up
                      setVBlank
                      setVar scanline 0
                      whenM nmiOnVBlank signalNmiInterrupt
                  | otherwise -> do -- life saver
                      alterVar scanline (+1)

genSLPixel :: PPU s ()
genSLPixel = do
    v1 <- liftM (.&. 0xFBE0) $ getVar vLoopy
    t1 <- liftM (.&. 0x41F)  $ getVar tLoopy
    setVar vLoopy (v1 .|. t1)

    whenM isBGVisible genBackground
--    whenM isSpriteVisible renderSprites

    setVar vLoopy $
        if (v1 .&. 0x7000) == 0x7000
            then let v2 = v1 .&. 0x8FFF
                 in if (v2 .&. 0x3E0) == 0x3A0
                       then (v2 `xor` 0x800) .&. 0xFC1F
                       else if (v2 .&. 0x3E0) == 0x3E0
                                then (v2 .&. 0xFC1F)
                                else v2 + 0x20
            else v1 + 0x1000

genBackground :: PPU s ()
genBackground = do
    v <- getVar vLoopy
    x <- xIndex
    y <- yIndex

    -- name table address
    ntAddr <- return $ 0x2000 + (v .&. 0xFFF)

    -- attribute table address
    atAddr <- let y' = (y .&. 0xFFC) `shiftL` 1
                  x' = x `shiftR` 2
              in return $ 0x2000 + 0x3C0 + y' + x' + (v .&. 0xC00)

    calcPatterns ntAddr atAddr x y

calcPatterns :: Address -> Address -> Address -> Address -> PPU s ()
calcPatterns ntAddr atAddr x y = do
    attrib <- calcAttribute atAddr x y

    patternAddr <- do 
        bgAddr <- bgTableAddress
        yOffset  <- liftM ((`shiftR` 12) . (.&. 7000)) $ getVar vLoopy
        tileBase <- liftM (`shiftL` 4)                 $ readPPUMem ntAddr
        return $ bgAddr + yOffset + (fromIntegral tileBase) -- check here

    patternLSB <- readPPUMem patternAddr
    patternMSB <- readPPUMem (patternAddr+8)

    forM_ [7,6..0] $ genTile patternLSB patternMSB attrib

    let ntAddr' = ntAddr + 1
    let x' = x + 1

    if (x' .&. 0x3) == 0
        then if (x' .&. 0x1F) == 0
                then calcPatterns ((ntAddr' `xor` 0x400) - 20)
                                  ((atAddr `xor` 0x400) - 8)
                                  (x'-8)
                                  y
                else calcPatterns ntAddr' (atAddr+1) x' y
        else calcPatterns ntAddr' atAddr x' y


calcAttribute :: Address -> Address -> Address -> PPU s Operand
calcAttribute atAddr idY idX = liftM bitMask (readPPUMem atAddr)
  where
    bY = idY `testBit` 1
    bX = idX `testBit` 1
    bitMask | bY     && bX     = (`shiftR` 4) . (.&. 0xC0)
            | bY     && not bX = (`shiftR` 2) . (.&. 0x30)
            | not bY && bX     = (.&. 0xC)
            | not bY && not bX = (`shiftL` 2) . (.&. 0x3)

genTile :: Operand -> Operand -> Operand -> Int -> PPU s ()
genTile plsb pmsb attr idx = do
    let pattern = (moveBit pmsb idx 2) + (moveBit plsb idx 1)
        colorAddr | pattern == 0 = 0x3F00
                  | otherwise    = 0x3F00 + (fromIntegral attr :: Address) -- *check here*:
    color <- liftM makeItRGB (readPPUMem colorAddr)
    -- PIXELBAJS
    return ()
