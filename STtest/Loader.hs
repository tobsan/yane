module Loader where

import CPUDefs
import Instr

import Data.Word
import Data.Int

import Control.Monad.Trans (lift)
import Control.Monad.ST

import System.IO (openBinaryFile, hFileSize, IOMode (ReadMode))
import Data.Array.IO

--import qualified Data.ByteString.Lazy as BS (readFile, index, length)

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

writeBinToMem :: [Operand] -> Address -> NES s ()
writeBinToMem ops offset = do
    let bs = zip [offset..] ops -- ++ zip [0x7000..] [0x20, 0x00, 0x70]
    mapM_ (\(a,op) -> writeMemory a op) bs
