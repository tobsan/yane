module Main where

import Types
import CPUDefs
import CPU
import CPUHelpers
import InstrSet
import Loader

import Data.Word
import Data.Int
import Numeric (showHex)

import qualified Control.Monad.ST as S
import Control.Monad.ST.Lazy
import Control.Monad.Reader
import Data.Array.ST
import Data.STRef.Lazy

import System.Environment (getArgs)

-- | Initial CPU State.
initCPU = CPUState
    { aReg   = 0x00
    , xReg   = 0x00
    , yReg   = 0x00
    , pc     = 0x6000
    , sp     = 0xFF
    , status = 0x0 -- c,z,i,d,b,v,n,q
    }

initSys :: ST s (SysEnv s)
initSys = do
    low <- newArray (0x0, 0x07FF) 0x0
    ppu <- newArray (0x2000, 0x2007) 0x0
    upp <- newArray (0x4020, 0xFFFF) 0xEA
    cpu <- newSTRef initCPU
    return $ SysEnv cpu low ppu upp undefined

loopSys :: [CPUState] -> Int -> CPU s (Int, [CPUState])
loopSys acc cc = do
    instr <- fetch
    if instr == 0xEA --NOP
       then do
         cpu <- getCPUState
         return (cc, (cpu:acc))
       else do -- execute instr >>= loopSys . (cc+)
         c <- execute instr
         cpu <- getCPUState
         loopSys (cpu:acc) (cc+c)

run :: [Operand] -> ST s (Int, [CPUState])
run ops = do --initSys >>= runReaderT loopSys >>= return
    sys <- initSys
    (count, cpu) <- runReaderT (writeBinToMem ops 0x6000 >> loopSys [] 0) sys
    return (count, cpu)

main :: IO ()
main = do
    args <- getArgs
    case args of
      [arg] -> do 
                ops <- loadBin arg
                let (count, cpu) = runST (run [0xA9,0x01])
                putStrLn $ show count
                putStrLn $ show cpu

debugRun c = do
    sys <- stToIO initSys
    stToIO $ runReaderT c sys
-- | Show in hexcode.
inHex :: Integral m => m -> String
inHex m = "0x" ++ showHex (fromIntegral m :: Word8) ""
