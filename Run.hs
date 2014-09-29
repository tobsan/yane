{-# LANGUAGE BangPatterns #-}
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

import System.IO
import System.Environment (getArgs)

import Debug.Trace

pcOffset = 0xE900

-- | Initial CPU State.
{-
initCPU :: ST s (SysEnv s)
initCPU = do
    aReg <- newSTRef 0x00
    xReg <- newSTRef 0x00
    yReg <- newSTRef 0x00
    sp   <- newSTRef 0xFF
    pc   <- newSTRef 0xE900
    status <- newSTRef 0x00
    return $ aReg xReg yReg sp pc status

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

loopSys :: Int -> CPU s Int
loopSys cc = do
    pc <- getPC
    instr <- fetch
    if instr == 0xEA --NOP
       then do
         return cc
       else do -- execute instr >>= loopSys . (cc+)
         !c <- trace (inHex pc ++ ":" ++ (inHex instr)) (execute instr)
         loopSys $! (cc+c)

run :: [Operand] -> ST s Int
run ops = do --initSys >>= runReaderT loopSys >>= return
    sys <- initSys
    runReaderT (writeBinToMem ops pcOffset >> loopSys 0) sys

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case args of
      [arg] -> do 
            ops <- loadBin arg
            let count = runST (run ops) -- [0xA0, 0xFF, 0xA2, 0xFF, 0xEA]) --[0xA9, 0x1, 0x3A, 0xD0, 0xFD, 0xEA] -- LDA 1, DECA, BNE -3, NOP
            print count

debugRun c = do
    sys <- stToIO initSys
    stToIO $ runReaderT c sys

-- | Show in hexcode.
inHex :: Integral m => m -> String
inHex m = "0x" ++ showHex (fromIntegral m :: Word16) ""
