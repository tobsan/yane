{-# LANGUAGE BangPatterns, TypeSynonymInstances #-}

module Main where


import Prelude hiding (init, Left, Right)
import Types
import CPUDefs
import PPUDefs
import CPU
import PPU
import Loader
import PPUHelpers (ppuDecodeAction,  getPPUActions, getVar, inHex)
import CPUHelpers (cpuDecodeActions, writePlayer, getCPUAction, handleIRQ, getPC)
import Data.Array.ST
import Control.Monad.Reader
import Debug.Trace
import System.IO
import System.Environment (getArgs)
-- import Numeric (showHex)
import NESDL
import Text.Printf
import System.CPUTime

import qualified Control.Monad.ST.Lazy as Lazy

type Cycle = Int
-- type LazyCPU s a = ReaderT (SysEnv s) (Lazy.ST s) a

{-
-- on keypress -> setBit
-- on keyrelease -> clearBit
-- FIXME maybe input race condition while reading?
insertKey :: Maybe Input -> CPU s ()
insertKey Nothing              = return ()
insertKey (Just (Input t p k)) = writePlayer (playerFun p) (pattern (getBit k) t)
 where
    getBit key = case key of 
        A -> 0; B -> 1; Select -> 2; Start -> 3;
        Up -> 4; Down -> 5; Left -> 6; Right -> 7
    playerFun player = if player == P1 then player1 else player2
    pattern i KeyPress = (`setBit` i) 
    pattern i KeyRelease = (`clearBit` i)

-}
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    sdlInit
    args <- getArgs
    case args of
        [arg] -> do
            rom <- loadRom arg
            ks  <- getKeys
            --let pxs = runSystem rom ks   -- unified looping
            let pxs = puller rom ks        -- enterprise pulling
            drawFrame pxs
        _ -> do fail "wrong number of arguments"
    forever (return ())
    print "LazyNES 1.0"

drawFrame :: [Pixel] -> IO ()
{-
drawFrame pxs = do
--    start <- getCPUTime
    let !pxs' =  drop (256*240) pxs
--    end <- getCPUTime
--    let diff = (fromIntegral (end - start)) / (10^12)
--    printf "Computation time: %0.3f sec\n" (diff :: Double)
    drawFrame pxs'
-}
drawFrame pxs = do
--    start <- getCPUTime
    draw frame
--    end <- getCPUTime
--    let diff = (fromIntegral (end - start)) / (10^12)
--    printf "Computation time: %0.3f sec\n" (diff :: Double)
    drawFrame rest
  where
    (frame, rest) = splitAt' (256*240) pxs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' 0 xs = ([], xs)
splitAt' i (x:xs) = (x:ys, zs)
  where (ys, zs) = splitAt' (i-1) xs

puller :: HDRiNES -> [Maybe Input] -> [Pixel]
puller (HDRiNES p c _ _) ks = pxs
  where (pxs, ppus) = execPPU c (execCPU p ks ([NOP]:ppus))

-- Like puller, but with unified looping instead
runSystem :: HDRiNES -> [Maybe Input] -> [Pixel]
runSystem (HDRiNES p c _ _) ks = Lazy.runST (do
    (cpu,ppu) <- Lazy.strictToLazyST $ initSys p c
    unified cpu ppu ks [NOP] )

-- runs the cpu and ppu as one unit.
-- needs to get the environments from the both units as input
-- as initCPU below
unified :: SysEnv s -> PPUEnv s -> [Maybe Input] -> [Action] -> Lazy.ST s [Pixel]
unified cpuenv ppuenv (inp:inps) acts = do
    c        <- Lazy.strictToLazyST $ stepCPU cpuenv inp acts
    (pxs,as) <- Lazy.strictToLazyST $ stepPPU ppuenv c
    pxss     <- unified cpuenv ppuenv inps as
    return $ pxs ++ pxss

execPPU :: [Operand] -> [(Cycle,Action)] -> ([Pixel],[[Action]])
execPPU chr c = Lazy.runST (do
    (_,env) <- Lazy.strictToLazyST $ initSys [] chr
    loopPPU env c)

execCPU :: [Operand] -> [Maybe Input] -> [[Action]] -> [(Cycle,Action)]
execCPU prg inp acts = Lazy.runST (do
    (env,_) <- Lazy.strictToLazyST $ initSys prg []
    loopCPU env inp acts)

-- Given PRGROM and CHRROM, initialize the environments for
-- CPU and PPU.
initSys :: [Operand] -> [Operand] -> ST s (SysEnv s,PPUEnv s)
initSys prg chr = do
    cpu <- return SysEnv
            `ap` newSTRef 0x0                    -- | A
            `ap` newSTRef 0x0                    -- | X
            `ap` newSTRef 0x0                    -- | Y
            `ap` newSTRef 0xFF                   -- | SP
            `ap` newSTRef 0x8000                 -- | PC
            `ap` newSTRef 0x0                    -- | Status
            `ap` newSTRef (Just Reset)           -- | IRQ
            `ap` newArray (0x0000, 0x07FF) 0x0   -- | Lower memory
            `ap` newArray (0x2000, 0x2007) 0x0   -- | PPU registers
            `ap` newArray (0x4000, 0xFFFF) 0x0   -- | Upper memory
            `ap` newSTRef NOP                    -- | Action
            `ap` newSTRef (InputState 0 0)       -- | Player 1
            `ap` newSTRef (InputState 0 0)       -- | Player 2
    ppu <- return PPUEnv
            `ap` newSTRef (PPUState 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)
            `ap` newArray (0x0000, 0x2000) 0x0       -- |
            `ap` newArray (0x2000, 0x3000) 0x0       -- |
            `ap` newArray (0x3F00, 0x3F20) 0x0       -- |
            `ap` newArray (0x00, 0xFF) 0x0           -- | SPR-RAM
            `ap` newSTRef 0                          -- |
            `ap` newSTRef 0                          -- |
            `ap` newSTRef 0                          -- |
            `ap` newSTRef 0                          -- |
            `ap` newSTRef 0                          -- |
            `ap` newSTRef 0                          -- |
            `ap` newSTRef []                         -- |
            `ap` newSTRef True                       -- |
            `ap` newSTRef []                         -- | [Action]
    writeBinToMemST (patternTables ppu) chr 0x0000
    writeBinToMemST (uppMem cpu) prg 0x8000
    return (cpu,ppu)

------------------------------------------------------------------------------
-- CPU

-- | (Lazy) Will call a strict step.
loopCPU :: SysEnv s -> [Maybe Input] -> [[Action]] -> Lazy.ST s [(Cycle, Action)]
loopCPU env (k:ks) (as:ass) = do
    cpu  <- Lazy.strictToLazyST (stepCPU env k as)
    cpus <- loopCPU env ks ass
    return $ cpu:cpus

-- | (Strict) step.
stepCPU :: SysEnv s -> Maybe Input -> [Action] -> ST s (Cycle, Action)
stepCPU env k as = flip runReaderT env $ do
    cpuDecodeActions as
    cycle  <- (handleIRQ >> fetch >>= execute)
    action <- getCPUAction
    return $ (cycle, action)

------------------------------------------------------------------------------
-- PPU

-- (Lazy) Will step a strict ppu.
loopPPU :: PPUEnv s -> [(Cycle, Action)] -> Lazy.ST s ([Pixel], [[Action]])
loopPPU env (x:xs) = do
    (pxs, as) <- Lazy.strictToLazyST $ stepPPU env x
    ~(pxs', as') <- loopPPU env xs
    return $ (pxs++pxs', as:as')

-- (Strict)
stepPPU :: PPUEnv s -> (Cycle, Action) -> ST s ([Pixel], [Action])
stepPPU env (cc,action) = flip runReaderT env $ do
    ppuDecodeAction action
    ps <- runPPU (cc * 3)
    a <- getPPUActions
    return (ps, a)
