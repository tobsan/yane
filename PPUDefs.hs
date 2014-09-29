module PPUDefs where

import Types 
import Control.Monad.Reader
import Data.Array.ST

type PPU s a = ReaderT (PPUEnv s) (ST s) a
type MemArray s = STUArray s Address Operand

data PPUEnv s = PPUEnv
    { ppuState      :: STRef s PPUState -- | PPU Registers

    -- | Memory
    , patternTables :: MemArray s       -- | Address range; [0x0000, 0x2000)
    , nameTables    :: MemArray s       -- | Address range; [0x2000, 0x3000)
    , palettes      :: MemArray s       -- | Address range; [0x3F00, 0x3F20)
    , ppuOAM        :: MemArray s       -- | OAM (also known as SPR-RAM)

    -- | Internal variables of the ppu.
    , scanline      :: STRef s Int      -- | Current scanline.
    , scanlineCycle :: STRef s Int      -- | Current clock cycle in scanline.
    , sprAddr       :: STRef s Address  -- | SRAM Address.
    , vLoopy        :: STRef s Address  -- | VRAM Address.
    , tLoopy        :: STRef s Address  -- | Temporary VRAM Address.
    , xLoopy        :: STRef s Address  -- | X-scroll.
    , pixels        :: STRef s [Pixel]  -- | Generated pixels
    , firstWrite    :: STRef s Bool     -- | First write to PPUSCROLL/PPUADDRESS.

    -- | Actions
    , ppuActions    :: STRef s [Action] -- | Actions perfomed.
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
