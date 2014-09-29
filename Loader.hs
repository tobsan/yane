module Loader where

import Types
import CPUDefs
import CPUHelpers

import Data.Word
import Data.Int

import Control.Monad.Trans (lift)
import Control.Monad.ST

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

data Name = Name {
      n :: Word8,
      e :: Word8,
      s :: Word8,
      b :: Word8
    }

data Padding = Padding {
      p0 :: Word8,
      p1 :: Word8,
      p2 :: Word8,
      p3 :: Word8,
      p4 :: Word8,
      p5 :: Word8,
      p6 :: Word8,
      p7 :: Word8
    }

data HDRiNES = HDRiNES {
      name  :: Name,
      prg   :: Word8,     -- 16KB units
      chr   :: Word8,     --  8KB units
      flag6 :: Flag6,
      flag7 :: Flag7,
      pad   :: Padding
    }

data Flag6 = Flag6 {
      
    }

data Flag7 = Flag7 {
      
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

loadiNES = undefined

loadBin :: FilePath -> IO [Operand]
loadBin file = do
    hndl <- openBinaryFile file ReadMode
    size <- hFileSize hndl
    arr <- newArray_ (0, fromIntegral size) -- :: IO (IOUArray Int Word8)
    hGetArray hndl arr $ fromIntegral size
    elems <- getElems arr
    return $ map fromIntegral elems

writeBinToMem :: [Operand] -> Address -> CPU s ()
writeBinToMem ops offset = do
    let bs = zip [offset..] ops -- ++ zip   [0x7000..] [0x20, 0x00, 0x70]
    mapM_ (\(a,op) -> writeMemory a op) bs
