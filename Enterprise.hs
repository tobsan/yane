{-# LANGUAGE BangPatterns, TypeSynonymInstances #-}

module Main where


import Prelude hiding (init, Left, Right)
import Types
import CPUDefs
import PPUDefs
import CPU
import PPU
import Loader
import PPUHelpers (ppuDecodeAction,  getPPUActions)
import CPUHelpers
import Data.Array.ST
import Control.Monad.Reader
import System.IO
import System.Environment (getArgs)
import NESDL

-- import CPUST
import CpuCPS

import qualified Control.Monad.ST.Lazy as Lazy

type Cycle = Int

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
            ks  <- return (repeat (error "key?!"))
            -- let pxs = runSystem rom ks   -- unified looping
            let pxs = puller rom ks         -- enterprise pulling
            drawFrame pxs
        _ -> do fail "wrong number of arguments"
    forever (return ())
    print "LazyNES 1.0"

drawFrame :: [Pixel] -> IO ()
drawFrame pxs = do
    draw frame
    drawFrame rest
  where
    (frame, rest) = splitAt (256*240) pxs

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
unified :: CPUEnv s -> PPUEnv s -> [Maybe Input] -> [Action] -> Lazy.ST s [Pixel]
unified _ _ [] _ = error "always some input"
unified cpuenv ppuenv (inp:inps) acts = do
    c        <- Lazy.strictToLazyST $ stepCPU cpuenv inp acts
    (pxs,as) <- Lazy.strictToLazyST $ stepPPU ppuenv c
    pxss     <- unified cpuenv ppuenv inps as
    return $ pxs ++ pxss

execPPU :: [Operand] -> [(Cycle,Action)] -> ([Pixel],[[Action]])
execPPU chrR c = Lazy.runST (do
    (_,env) <- Lazy.strictToLazyST $ initSys [] chrR
    loopPPU env c)

execCPU :: [Operand] -> [Maybe Input] -> [[Action]] -> [(Cycle,Action)]
execCPU prgR inp acts = Lazy.runST (do
    (env,_) <- Lazy.strictToLazyST $ initSys prgR []
    loopCPU env inp acts)

-- Given PRGROM and CHRROM, initialize the environments for
-- CPU and PPU.
initSys :: [Operand] -> [Operand] -> ST s (CPUEnv s,PPUEnv s)
initSys prgR chrR = do
    cpu <- return CPUEnv
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
    writeBinToMemST (patternTables ppu) chrR 0x0000
    writeBinToMemST (uppMem cpu) prgR 0x8000
    return (cpu,ppu)

------------------------------------------------------------------------------
-- CPU

-- | (Lazy) Will call a strict step.
loopCPU :: CPUEnv s -> [Maybe Input] -> [[Action]] -> Lazy.ST s [(Cycle, Action)]
loopCPU _ [] _ = error "Always input"
loopCPU _ _ [] = error "Always more actions"
loopCPU env (k:ks) (as:ass) = do
    cpu  <- Lazy.strictToLazyST (stepCPU env k as)
    cpus <- loopCPU env ks ass
    return $ cpu:cpus

-- | (Strict) step.
stepCPU :: CPUEnv s -> Maybe Input -> [Action] -> ST s (Cycle, Action)
stepCPU env _ as = runCpuCPS m return env
-- flip runReaderT env $ runCPUST m
  where 
    m = do handlePPUAction as
           c <- (handleIRQ >> fetch >>= execute)
           a <- getCPUAction
           return $ (c, a)

------------------------------------------------------------------------------
-- PPU

-- (Lazy) Will step a strict ppu.
loopPPU :: PPUEnv s -> [(Cycle, Action)] -> Lazy.ST s ([Pixel], [[Action]])
loopPPU _ [] = error "Always more"
loopPPU env (x:xs) = do
    (pxs, as) <- Lazy.strictToLazyST $ stepPPU env x
    ~(pxs', as') <- loopPPU env xs
    return $ (pxs++pxs', as:as')

-- (Strict)
stepPPU :: PPUEnv s -> (Cycle, Action) -> ST s ([Pixel], [Action])
stepPPU env (cc,a) = flip runReaderT env $ do
    ppuDecodeAction a
    ps <- runPPU (cc * 3)
    as <- getPPUActions
    return (ps, as)
