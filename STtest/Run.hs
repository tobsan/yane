module Main where

import CPUDefs
import CPU
import InstrSet
import Instr
import Loader

import Data.Word
import Data.Int
import Numeric (showHex)

import Control.Monad.ST
import Control.Monad.Reader
import Data.Array.ST
import Data.STRef

import System.Environment (getArgs)

-- | Initial CPU State.
initCPU = CPU
    { aReg   = 0x00
    , xReg   = 0x00
    , yReg   = 0x00
    , pc     = 0xE900
    , sp     = 0xFF
    , status = Status False False False False False False False False -- c,z,i,d,b,v,n,q
    }

-- | Initial Memory
{-
initMEM :: Memory s
initMEM = Memory 
    { lowMem = (newArray (0x0, 0x07FF) 0x0 ) :: STUArray s Address Operand
--    , ppu    = newArray (0x2000, 0x2007) 0x0
--    , uppMem = newArray (0x4020, 0xFFFF) 0xEA -- NOPs
    }
-}

initSys :: ST s (Sys s)
initSys = do
    low <- newArray (0x0, 0x07FF) 0x0
    ppu <- newArray (0x2000, 0x2007) 0x0
    upp <- newArray (0x4020, 0xFFFF) 0xEA
    cpu <- newSTRef initCPU
    mem <- newSTRef (Memory low ppu upp)
    return $ Sys cpu mem

loopSys :: Int -> NES s (Int, CPU)
loopSys cc = do
    instr <- fetch
--    writeMemory 0x4016 0x1 -- write to Joypad Reg
    if instr == 0xEA --NOP
       then do
         cpu <- getCPU
         return $! (cc, cpu)
       else do -- execute instr >>= loopSys . (cc+)
         c <- execute instr
         cpu <- getCPU
         --(c, cpus) <- loopSys cc lcpu
         loopSys $! (cc+c)

run :: [Operand] -> ST s (Int, CPU)
run ops = do --initSys >>= runReaderT loopSys >>= return
    sys <- initSys
    (count, cpu) <- runReaderT (writeBinToMem ops 0xE900 >> loopSys 0) sys
    return (count, cpu)

main :: IO ()
main = do
    args <- getArgs
    case args of
      [arg] -> do 
                ops <- loadBin arg
                let (count, cpu) = runST (run ops)
                putStrLn $ show count
                putStrLn $ show cpu

-- | Show in hexcode.
inHex :: Integral m => m -> String
inHex m = "0x" ++ showHex (fromIntegral m :: Word8) ""
