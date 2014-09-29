module CPUDefs where

import Types
import Control.Monad.Reader
import Data.Array.ST

type CPU s a = ReaderT (SysEnv s) (ST s) a
type Memory s = STArray s Address Operand

-- | The Environment storing the STRefs.
data SysEnv s = SysEnv {
      aReg     :: STRef s Operand
    , xReg     :: STRef s Operand
    , yReg     :: STRef s Operand
    , sp       :: STRef s Word8
    , pc       :: STRef s Address
    , status   :: STRef s Operand

    , irq      :: STRef s (Maybe IRQ)

    , lowMem   :: Memory s    -- ^ Memory range  0    - 7FF
    , ppuMem   :: Memory s    -- ^ Memory range  2000 - 2007
    , uppMem   :: Memory s    -- ^ Memory range  4000 - FFFF
    , action   :: STRef s Action
    , player1  :: STRef s InputState
    , player2  :: STRef s InputState
}

-- The state that keeps track of user input
data InputState = InputState { register :: Operand , readNum :: Int }

-- | The stacks starting position.
baseSP :: Address
baseSP = 0x0100

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

