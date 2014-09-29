{-# LANGUAGE TypeSynonymInstances #-}

import Control.Monad.State.Strict
import Test.QuickCheck
import Data.Word
import Data.Bits
import Data.Array.Unboxed
import Instr
import InstrSet
import CPUDefs

instance Arbitrary CPU where
    coarbitrary = undefined
    arbitrary = do
        aReg    <- arbitrary
        xReg    <- arbitrary
        yReg    <- arbitrary
        sp      <- arbitrary
        pc      <- arbitrary
        status  <- arbitrary
        mem     <- arbitrary
        return $ CPU aReg xReg yReg sp pc status mem

instance Arbitrary Operand where
    coarbitrary = undefined
    arbitrary = do
        let mx = maxBound :: Int
        let mn = minBound :: Int
        c <- choose (mx,mn)
        return $ fromIntegral c

instance Arbitrary Address where
    coarbitrary = undefined
    arbitrary = do
        low     <- arbitrary
        high    <- arbitrary
        return $ mkAddr low high

instance Arbitrary Status where
    coarbitrary = undefined
    arbitrary = do
        c <- arbitrary
        z <- arbitrary
        i <- arbitrary
        d <- arbitrary
        b <- arbitrary
        v <- arbitrary
        n <- arbitrary
        return $ Status c z i d b v n

instance Arbitrary Memory where
    coarbitrary = undefined
    arbitrary = do
        valLow      <- arbitrary
        valPPU      <- arbitrary
        valHigh     <- arbitrary
        let mem = Memory {
            lowMem  = array (0x0000,0x07FF) $ zip [0x0000..0x07FF] valLow,
            ppu     = array (0x2000,0x2007) $ zip [0x2000..0x2007] valPPU,
            uppMem  = array (0x4000,0xFFFF) $ zip [0x4000..0xFFFF] valHigh,
            dma     = 0x0000
        } in return $ mem { dma = (uppMem mem) ! 0x4014 }


-- tests that pull (push op) == op
prop_pull_val :: CPU -> Operand -> Bool
prop_pull_val cpu op = evalState (push op >> pull) cpu == op

-- tests that sp is restored when pulling after pushing
prop_pull_sp :: CPU -> Operand -> Bool
prop_pull_sp cpu op = sp cpu == sp state
    where
        state = execState (push op >> pull) cpu

-- toOper(fromOper oper) == oper
prop_oper :: Operand -> Bool
prop_oper op = clearBit op 5 == toOper (fromOper op)

-- readMemory (writeMemory op) == op
prop_mem :: CPU -> Operand -> Address -> Bool
prop_mem cpu op addr = evalState (writeMemory addr op >> readMemory addr) cpu == op

-- tests the mirroring
prop_mirror :: CPU -> Operand -> Address -> Bool
prop_mirror cpu op addr = prop_mem cpu op (0x2000 + (addr `mod` 0x2000))

-- testing mkAddr
prop_addr :: Operand -> Operand -> Bool
prop_addr low high = low' == low && high' == high
    where
        low'    = fromIntegral $ (mkAddr low high) .&. 0xFF
        high'   = fromIntegral $ (mkAddr low high) `shiftR` 8

-- Test suite :o
qcMain :: IO ()
qcMain = do
    putStrLn "Testing write/read memory"
    quickCheck prop_mem
    putStrLn "Testing mirroring of memory"
    quickCheck prop_mirror
    putStrLn "Testing push/pull"
    quickCheck prop_pull_val
    quickCheck prop_pull_sp
    putStrLn "Testing mkAddr"
    quickCheck prop_addr
    putStrLn "Testing toOper/fromOper"
    quickCheck prop_oper
    putStrLn "All done"
