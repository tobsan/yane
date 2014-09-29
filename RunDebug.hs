module Main where

import Types
import CPUDefs
import CPU
import CPUHelpers
import InstrSet
import Loader

import Numeric (showHex)

import Control.Monad.Reader
import Data.Array.ST

import System.Environment (getArgs)

import Debug.Trace

pcOffset = 0xC000

{-
initSys :: ST s (SysEnv s)
initSys = do
    aReg <- newSTRef 0x00
    xReg <- newSTRef 0x00
    yReg <- newSTRef 0x00
    sp   <- newSTRef 0xFF
    pc   <- newSTRef pcOffset
    status <- newSTRef 0x00

    low <- newArray (0x0, 0x07FF) 0x0
    ppu <- newArray (0x2000, 0x2007) 0x0
    upp <- newArray (0x4020, 0xFFFF) 0xEA
    return $ SysEnv aReg xReg yReg sp pc status low ppu upp undefined
-}

initSys :: ST s (SysEnv s)
initSys = do
    return SysEnv 
          `ap` newSTRef 0x0                    -- | A
          `ap` newSTRef 0x0                    -- | X
          `ap` newSTRef 0x0                    -- | Y
          `ap` newSTRef 0xFF                   -- | SP
          `ap` newSTRef pcOffset               -- | PC
          `ap` newSTRef 0x0                    -- | Status
          `ap` newSTRef (Just Reset)           -- | IRQ
          `ap` newArray (0x0000, 0x07FF) 0xEA  -- | Lower memory
          `ap` newArray (0x2000, 0x2007) 0xEA  -- | PPU registers
          `ap` newArray (0x4000, 0xFFFF) 0xEA  -- | Upper memory
          `ap` newSTRef NOP                    -- | Action
          `ap` newSTRef (InputState 0 0)       -- | Player 1
          `ap` newSTRef (InputState 0 0)       -- | Player 2

loopSys :: [Operand] -> Int -> CPU s (Int, [Operand])
loopSys acc cc = do
    instr <- fetch
    if instr == 0xEA --NOP
       then do
--         cpu <- trace (inHex instr) getCPUState
         a <- trace (inHex instr) getA
         return (cc, (a:acc))
--         loopSys (a:acc) (cc)
       else do -- execute instr >>= loopSys . (cc+)
         c <- trace (inHex instr) (execute instr)
--         cpu <- getCPUState
         a <- getA
         loopSys (a:acc) (c+cc)

run :: [Operand] -> ST s (Int, [Operand])
run ops = do --initSys >>= runReaderT loopSys >>= return
    sys <- initSys
    (count, cpu) <- runReaderT (writeBinToMem ops pcOffset >> loopSys [] 0) sys
    return (count, cpu)

main :: IO ()
main = do
    args <- getArgs
    case args of
      [arg] -> do 
--                ops <- loadBin arg
                (HDRiNES ops chr _ _) <- loadRom arg
--                print $ map inHex $ take 10 ops
--                fail "boeg"
                let (count, cpu) = runST (run ops) --(run [0xA0, 0xFF, 0xA2, 0xFF, 0xA9, 0xFF, 0xEA])
                print count
                print $ take 10 $ map inHex cpu


-- | Show in hexcode.
inHex :: Integral m => m -> String
inHex m = "0x" ++ showHex (fromIntegral m :: Word8) ""
