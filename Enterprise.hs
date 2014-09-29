{-# LANGUAGE BangPatterns #-}
module Enterprise where

import Types
import CPU
import CPUDefs
import PPU
import PPUHelpers (ppuHandleAction)
import PPUDefs
import Data.Array.ST
import Control.Monad.Reader
import Control.Monad.ST.Lazy
import Data.STRef.Lazy

type Cycle = Int
type Keys = Int
-- 
insertKey = undefined
cpuHandleAction = undefined

getCPUAction = undefined
getPPUAction = undefined
-- 

puller :: [Maybe Keys] -> [Pixel]
puller ks = let (pxs, ppus) = runPPU' (runCPU ks ([NOP]:ppus))
            in pxs

runCPU :: [Maybe Keys] -> [[Action]] -> [(Cycle, Action)]
runCPU ks' as' = runST (do
    fail "run CPU!"
    initCPU <- do
        low <- newArray (0x0, 0x07FF) 0x0
        ppu <- newArray (0x2000, 0x2007) 0x0
        upp <- newArray (0x4020, 0xFFFF) 0xEA
        cpu <- newSTRef (CPUState 0x00 0x00 0x00 0xFF 0xE900 0x00)
        act <- newSTRef NOP
        return $ SysEnv cpu low ppu upp act
    runReaderT (loopCPU ks' as') initCPU
    )
  where
    loopCPU (k:ks) (a:as) = do
        insertKey k
        cpuHandleAction a
        cycle <- (fetch >>= execute)
        action <- getCPUAction
        cpus <- loopCPU ks as
        return $ (cycle, action):cpus

runPPU' :: [(Cycle, Action)] -> ([Pixel], [[Action]])
runPPU' inp = runST (do
    fail "run PPU!"
    ppuInit <- do
        sta <- newSTRef (PPUState 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00)
        pat <- newArray (0x0, 0x2000) 0x0
        nam <- newArray (0x2000, 0x3000) 0x0
        pal <- newArray (0x3F00, 0x3F20) 0xEA
        oam <- newArray (0x0, 0xFF) 0xEA
        sc  <- newSTRef 0
        scc <- newSTRef 0
        spr <- newSTRef 0
        vl  <- newSTRef 0
        tl  <- newSTRef 0
        xl  <- newSTRef 0
        bck <- newSTRef []
        fst <- newSTRef True
        pl  <- newSTRef 0
        return $ PPURefs sta pat nam pal oam sc scc spr vl tl xl bck fst pl
    runReaderT (loopPPU inp) ppuInit
    )
  where
    loopPPU ((cc, action):xs) = do
        ppuHandleAction action
        ps <- runPPU cc
        a <- getPPUAction
        (ps', as) <- loopPPU xs
        return (ps++ps' , a:as)
