module Loader where

import Types
import Data.Array.ST
import System.IO (openBinaryFile, hFileSize, IOMode (ReadMode))
import Data.Array.IO

--import qualified Data.ByteString.Lazy as BS (readFile, index, length)
{-
    +--------+------+------------------------------------------+
    | Offset | Size | Content(s)                               |
    +--------+------+------------------------------------------+
    |   0    |  3   | NES                                      |
    |   3    |  1   | $1A                                      |
    |   4    |  1   | 16K PRG ROM banks                        |
    |   5    |  1   | 8K CHR ROM banks                         |
    |   6    |  1   | RAW control lower byte                   |
    |        |      |   %mmmmvTsM                              |
    |        |      |    |  ||||+- 0=Horizontal Mirroring      |
    |        |      |    |  ||||   1=Vertical Mirroring        |
    |        |      |    |  |||+-- 1=Backed battery            |
    |        |      |    |  ||+--- 1=Trainer data (512 bytes)  |
    |        |      |    |  |+---- 1=Four screen mirroring     |
    |        |      |    +--+----- Mapper # (lower 4-bits)     |
    |   7    |  1   | RAW control upper byte                   |
    |        |      |   %MMMM00PU                              |
    |        |      |    |  |  |+- 1=VS Unisystem arcade       |
    |        |      |    |  |  +-- 1=Playchoice-10 arcade      |
    |        |      |    +--+----- Mapper # (upper 4-bits)     |
    |  8-15  |  8   | Reserved, must be zeroes.                |
    +--------+------+------------------------------------------+

 * RAW stands for the PRG and CHR ROM banks only.
 * If four screen mirroring flag is set, then the horizontal/vertical is ignored.
 * PRG-ROM stands for PRoGram ROM, or 65O2 CPU instructions.
 * CHR-ROM stands for CHaRacter ROM, or game graphics.
-}

data HDRiNES = HDRiNES
    { prg      :: [Operand]
    , chr      :: [Operand]
    , ctrlLow  :: Word8
    , ctrlHigh :: Word8
    }

-- | Flag 6
{-
76543210
||||||||
||||+||+- 0xx0: horizontal mirroring; 0xx1: vertical mirroring; 1xxx: four-screen mirroring
|||| |+-- 1: SRAM in CPU $6000-$7FFF, if present, is battery backed
|||| +--- 1: 512-byte trainer at $7000-$71FF (stored before PRG data)
++++----- Lower nybble of mapper number
-}

-- | Flag 7
{-
76543210
||||  ||
||||  |+- VS Unisystem
||||  +-- PlayChoice-10 (8KB of Hint Screen data stored after CHR data)
++++----- Upper nybble of mapper number
-}

type Offset = Int

-- *TODO* use bytestring :)
loadRom :: FilePath -> IO HDRiNES
loadRom file = do
    hndl <- openBinaryFile file ReadMode
 --   hSeek hndl AbsoluteSeek 15
    size <- hFileSize hndl
    arr <- newArray_ (0, fromIntegral size) -- :: IO (IOUArray Int Word8)
    hGetArray hndl arr $ fromIntegral size
    b <- checkHeader arr
    if b
        then do

            prgC <- arr `readArray` 4 -- :: Word8
            let prgSize = 16 + (fromIntegral prgC)*1024*16
            prgRom <- mapM (\e -> arr `readArray` e >>= return . fromIntegral) [16..(prgSize-1)]
            let prgR = if prgC == 1
                          then prgRom ++ prgRom
                          else prgRom

            chrC <- arr `readArray` 5
            let chrSize = prgSize + (fromIntegral chrC)*1024*8
            chrR <- mapM (\e -> arr `readArray` e >>= return . fromIntegral) [prgSize..(chrSize-1)]

            low <- arr `readArray` 6
            high <- arr `readArray` 7

            return $ HDRiNES prgR chrR low high

        else do fail "no iNES"

loadBin :: FilePath -> IO [Operand]
loadBin file = do
    hndl <- openBinaryFile file ReadMode
    size <- hFileSize hndl
    arr <- newArray_ (0, fromIntegral size)
    hGetArray hndl arr $ fromIntegral size
    elems <- getElems arr
    return $ map fromIntegral elems

writeBinToMemST :: STUArray s Address Operand -> [Operand] -> Address -> ST s ()
writeBinToMemST arr ops offset = do
    let bs = zip [offset..] ops
    mapM_ (\(a,op) -> writeArray arr a op) bs

checkHeader :: (IOUArray Int Word8) -> IO Bool
checkHeader arr = do
    list <- mapM (arr `readArray`) [0,1,2,3]
    return $ list == [0x4E, 0x45, 0x53, 0x1A]
