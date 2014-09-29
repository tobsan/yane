module Main where

{-# LANGUAGE XBangPatterns #-}

import CPU
import CPUDefs
import InstrSet
import Instr
import Control.Monad.State.Strict
import Control.Concurrent (threadDelay)
import Data.Array.Unboxed (listArray, (!))
import Numeric (showHex)
import Data.Int
import Data.Word
import System.IO.Unsafe
import Data.IORef
import Bubble
-- import Bubble16

import System.IO --(openBinaryFile, hFileSize, IOMode)
--import qualified Data.ByteString.Lazy as BS (readFile, index, length)
import System.Environment (getArgs)
import Data.Array.IO

data LoopState = LoopState
    { tickCounter  :: ! Int
    , cycleCounter :: ! Int
    , state        :: ! CPU
    }

type NesLoop a = StateT LoopState IO a

main :: IO ()
main = do
    args <- getArgs
    case args of
      [arg] -> do
                handle <- openBinaryFile arg ReadMode
                fsize <- hFileSize handle
                arr <- newArray_ (offset, offset + fromIntegral fsize) :: IO (IOUArray Int Word8)
                hGetArray handle arr $ fromIntegral fsize
                assocs <- getAssocs arr
                let xss = map (\xs -> (fromIntegral $ fst xs, fromIntegral $ snd xs)) assocs

                let bs = writeToMem initCPU (  xss ++ zip [0x7000..] [0x20, 0x00, 0xE9] ) --E9 for stress.hex

-- Bubble Stuff
{-                let nums = reverse [1..127]
                let len = length $ bubbleData nums
                let bs = writeToMem initCPU (  xss
                                            ++ bubbleData nums
                                            ++ zip [0x7000..] [0x20, 0x00, 0x50] )
-}


                dat <- run bs 0
                putStrLn $ show dat

                --elems <- getElems arr
                --putStrLn $ show $ map inHex elems

--                inStream <- BS.readFile arg
--                putStrLn $ show $ BS.length inStream
                --writeToMem 
                --hClose inStream
                --print $ BS.index inStream 0  
                return ()
      _ -> do
         putStrLn "Usage: run <hexFile>"

    where offset = 0xE900

mainOld :: IO ()
mainOld = do putStrLn . show . bubbleSort . reverse $ [1..127]

-- | Run CPU with a memory and if bubbleSort  then specify the
--   number of elements.
run :: CPU -> Int -> IO [Operand]
run c n = do
    -- Looper with StateT LoopState IO
    putStr "StateT "
    res <- execStateT looper cpu
    putStrLn "... done"

    return $ getElements (state res)
  where
    !cpu = LoopState 0 0 c
    getElements = evalState (sequence [ readMemory idx
                                      | idx <- take n [0x6001..]])

writeToMem :: CPU -> [(Address, Int8)] -> CPU
writeToMem c d = execState (mapM_ (uncurry writeMemory) d) $ c

-- | Initial CPU State.
initCPU = CPU
    { aReg   = 0x00
    , xReg   = 0x00
    , yReg   = 0x00
    , pc     = 0x7000
    , sp     = 0xFF
    , status = Status False False False False False False False False -- c,z,i,d,b,v,n,q
    , mem    = Memory
        { lowMem = listArray (0x0   , 0x07FF) [0,0..]
        , ppu    = listArray (0x2000, 0x2007) [0,0..]
        , dma    = 0x0
        , uppMem = listArray (0x4020,0xFFFF) [0xEA,0xEA..] -- NOPs
        }
    }


-- *** Different initial CPU states
-- | Writes bubble sort to memory and a call to the sub routine.
bubbleSort xs = unsafePerformIO $ run bs (length xs)
  where bs = writeToMem initCPU (  bubble 0x5000
                                ++ bubbleData xs
                                -- jsr at 0x5000
                                ++ zip [0x7000..] [0x20, 0x00, 0x50])

-- | Empty memory
normalRun = run initCPU 0

------------------------------------------------------------------------------
-- | Looper with StateT
looper :: NesLoop ()
looper = do
    op <- getOP
    printOP
    printStatus
--    printCycle
    lift . putStr $ " "
    if op == 0xea -- if nop then quit
        then do 
--          printMem
          printCycle
--          printStatus
          return ()
        else do
            (cycles, cpu') <- fetchExecute
            modify $ \s -> s { state = cpu'
                             , cycleCounter = cycleCounter s + cycles }
            looper
  where
    printOP, printTick, printCycle, printStatus, printMem :: NesLoop ()
    printOP       = getOP             >>= lift . putStr . inHex
    printTick     = gets tickCounter  >>= lift . putStr . (++"\t") .  show
    printCycle    = gets cycleCounter >>= lift . putStrLn . show
    printStatus   = gets state >>= lift . putStr . show
    printMem = getMem >>= lift . putStr . show

    getMem = gets state >>= return . evalState (readMemory 0x60) --Stress Score Add.
    getOP = gets state >>= return . evalState (gets pc >>= readMemory)
    fetchExecute = gets state >>= return . runState (fetch >>= execute)

-- | Show in hexcode.
inHex :: Integral m => m -> String
inHex m = "0x" ++ showHex (fromIntegral m :: Word8) ""

