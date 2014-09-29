module CPUDefs where

import Data.Int
import Data.Word
import Types
import Control.Monad.Reader
import Control.Monad.ST.Lazy
import Data.STRef.Lazy
import Data.Array.ST
import Numeric (showHex)

type CPU s a = ReaderT (SysEnv s) (ST s) a
type Memory s = STArray s Address Operand

-- | The Environment storing the STURefs.
data SysEnv s = SysEnv {
      cpuState :: STRef s CPUState
    , lowMem   :: Memory s    -- ^ Memory range  0    - 7FF
    , ppuMem   :: Memory s    -- ^ Memory range  2000 - 2007
    , uppMem   :: Memory s    -- ^ Memory range  4000 - FFFF
    , action   :: STRef s Action
    }

-- | The stacks starting position.
baseSP :: Address
baseSP = 0x0100

------------------------------------------------------------------------------
-- | CPU State that keeps track of registers and status flags.
data CPUState = CPUState
   { aReg     :: !Operand
   , xReg     :: !Operand
   , yReg     :: !Operand
   , sp       :: !Word8
   , pc       :: !Address
   , status   :: !Operand
   }

instance Show CPUState where
    show cpu = " A=" ++ toHex (aReg cpu)
            ++ " | X=" ++ toHex (xReg cpu)
            ++ " | Y=" ++ toHex (yReg cpu)
            ++ " | PC=" ++ toHex (pc cpu)
            ++ " | SP=" ++ toHex (sp cpu)
            ++ " | " ++ show (status cpu)
--            ++ " | Stack "++ show [ (i, (lowMem $ mem cpu) ! i) | i <- [0x100..0x1FF]]
            ++ " |\n"
        where
          toHex m = showHex (fromIntegral m :: Word16) "h"

------------------------------------------------------------------------------
-- | Addressing modes.
-- Before = X, After = Y (apparently)
data Indexed = Before | After
data Index   = X | Y

data AddrMode =
     ZeroPage (Maybe Index) Operand
   | Absolute (Maybe Index) Address
   | Relative Operand
   | Indirect Address
   | Indexed Indexed Operand

