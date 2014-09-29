module Types (module Types, module Data.Int, module Data.Word, 
              module Data.Bits, module Data.IORef, module Data.Array.IO) where

import Data.Int
import Data.Word
import Data.Bits
import Data.IORef
import Data.Array.IO
import Control.Monad.Reader

type Operand    = Int8
type OPCode     = Int8
type Address    = Word16
type Pixel      = Word32 -- | RGB Value in hex.
type CPU a      = ReaderT CPUEnv IO a
type PPU a      = ReaderT PPUEnv IO a
type NES a      = ReaderT (CPUEnv,PPUEnv) IO a
type Memory     = IOUArray Address Operand

data IRQ
    = Normal -- from some mappers (no support yet).
    | NMI    -- Non-maskable interrupt.
    | Reset  -- ja du fattar.

-- cast an Int8 to a Word16 without promotion of sign
transform :: Operand -> Address
transform op = (fromIntegral op) .&. 0x00FF

-- Input handling
data Input      = Input InType Player Key deriving Show
data InType     = KeyPress | KeyRelease deriving Show
data Player     = P1 | P2 deriving (Show,Eq)
data Key        = A | B | Left | Up | Down | Right | Select | Start deriving Show
data InputState = InputState { register :: Operand , readNum :: Int }

-- | The Environment storing the STRefs.
data CPUEnv = CPUEnv {
      aReg     :: IORef Operand
    , xReg     :: IORef Operand
    , yReg     :: IORef Operand
    , sp       :: IORef Word8
    , pc       :: IORef Address
    , flags    :: IORef Operand

    , irq      :: IORef (Maybe IRQ)

    , lowMem   :: Memory        -- ^ Memory range  0    - 7FF
    , ppuMem   :: IORef Memory  -- ^ Memory range  2000 - 2007
    , uppMem   :: Memory        -- ^ Memory range  4000 - FFFF
    , player1  :: IORef InputState
    , player2  :: IORef InputState
}


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

data PPUEnv = PPUEnv
    { ppuState      :: IORef PPUState -- | PPU Registers

    -- | Memory
    , patternTables :: Memory       -- | Address range; [0x0000, 0x2000)
    , nameTables    :: Memory       -- | Address range; [0x2000, 0x3000)
    , palettes      :: Memory       -- | Address range; [0x3F00, 0x3F20)
    , ppuOAM        :: Memory       -- | OAM (also known as SPR-RAM)

    -- | Internal variables of the ppu.
    , scanline      :: IORef Int      -- | Current scanline.
    , scanlineCycle :: IORef Int      -- | Current clock cycle in scanline.
    , sprAddr       :: IORef Address  -- | SRAM Address.
    , vLoopy        :: IORef Address  -- | VRAM Address.
    , tLoopy        :: IORef Address  -- | Temporary VRAM Address.
    , xLoopy        :: IORef Address  -- | X-scroll.
    , pixels        :: IORef [Pixel]  -- | Generated pixels
    , firstWrite    :: IORef Bool     -- | First write to PPUSCROLL/PPUADDRESS.
   }

data PPUState = PPUState
    { control1      :: Operand -- | Address: $2000
    , control2      :: Operand -- | Address: $2001
    , status        :: Operand -- | Address: $2002
    , oamAddr       :: Operand -- | Address: $2003
    , oamData       :: Operand -- | Address: $2004
    , ppuScroll     :: Operand -- | Address: $2005
    , ppuAddr       :: Operand -- | Address: $2006
    , ppuData       :: Operand -- | Address: $2007
    }
