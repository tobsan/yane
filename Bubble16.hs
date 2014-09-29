module Bubble16 where
import CPUDefs
import Data.Int
import Data.Word

bubble16Hex :: [Int8]
bubble16Hex = -- SORT16
              [0xa0, 0x00 -- LDA #00
              ,0x84, 0x32 -- STY $32
              ,0xb1, 0x30 -- LDA ($30),Y
              ,0xa8       -- TAY
              -- NXTEL
              ,0xb1 ,0x30 -- LDA ($30),Y
              ,0x48       -- PHA
              ,0x88       -- DEY
              ,0xb1, 0x30 -- LDA ($30),Y
              ,0x38       -- SEC
              ,0x88       -- DEY
              ,0x88       -- DEY
              ,0xf1, 0x30 -- SBC ($30),Y
              ,0x68       -- PLA
              ,0xc8       -- INY
              ,0xf1, 0x30 -- SBC ($30),Y
              ,0x90, 0x09 -- BCC +9  (SWAP)
              ,0xc0, 0x02 -- CPY
              ,0xd0, 0xeb -- BNE -21 (NXTEL)
              ,0x24, 0x32 -- BIT $32
              ,0x30, 0xe0 -- BMI -32 (SORT16)
              ,0x60       -- RTS
              -- SWAP
              ,0xb1, 0x30 -- LDA ($30),Y
              ,0x48       -- PHA
              ,0x88       -- DEY
              ,0xb1, 0x30 -- LDA ($30),Y
              ,0x48       -- PHA
              ,0xc8       -- INY
              ,0xc8       -- INY
              ,0xc8       -- INY
              ,0xb1, 0x30 -- LDA ($30),Y
              ,0x48       -- PHA
              ,0x88       -- DEY
              ,0xb1 ,0x30 -- LDA ($30),Y
              ,0x88       -- DEY
              ,0x88       -- DEY
              ,0x91, 0x30 -- STA ($30),Y
              ,0xa2, 0x03 -- LDX #3
              -- SLOOP
              ,0xc8       -- INY
              ,0x68       -- PLA
              ,0x91, 0x30 -- STA ($30),Y
              ,0xca       -- DEX
              ,0xd0, 0xf9 -- BNE -7 (SLOOP)
              ,0xa9, 0xff -- LDA #FF
              ,0x85, 0x32 -- STA $32
              ,0xc0, 0x04 -- CPY
              ,0xf0, 0xba -- BEQ -70 (SORT16)
              ,0x88       -- DEY
              ,0x88       -- DEY
              ,0x4c, 0x07, 0x50] -- JMP $5007 (NXTEL)

bubble16 :: Address -> [(Address, Operand)]
bubble16 offset = zip [offset..] bubble16Hex

-- Bubblesort data stored at 0x6000 and onwards.
bubbleData16 :: [Operand] -> [(Address, Operand)]
bubbleData16 xs = [(0x30, 0x00) , (0x31, 0x60)]
             ++ (0x6000, fromIntegral $ (length xs `div` 2)) : zip [0x6001..] xs


