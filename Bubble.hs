module Bubble where
import CPUDefs
import Instr
import InstrSet
import Word
import Int

-- THIS SUBROUTINE ARRANGES THE 8-BIT ELEMENTS OF A LIST IN ASCENDING
-- ORDER.  THE STARTING ADDRESS OF THE LIST IS IN LOCATIONS $30 AND
-- $31.  THE LENGTH OF THE LIST IS IN THE FIRST BYTE OF THE LIST.  LOCATION
-- $32 IS USED TO HOLD AN EXCHANGE FLAG.

-- Bubblesort sub rutine at given starting address.
bubble adr = sort8 ++ nxtel ++ chkend
  where
    sort8adr = adr
    nxteladr = (+1) . fst . last $ sort8
    chkendadr = (+1) . fst . last $ nxtel

    sort8 = zip [sort8adr..]
        [ 0xA0, 0x00  -- ldy #$00     -- Turn exchange flag off (= 0)
        , 0x84, 0x32  -- sty $32
        , 0xB1, 0x30  -- lda ($30),Y  -- Fetch element count
        , 0xAA        -- tax          -- and put it in X
        , 0xC8        -- iny          -- point to first element in list
        , 0xCA        -- dex          -- decrement element count
        ] :: [(Address, Operand)]

    nxtel = zip [nxteladr..]
        [ 0xB1, 0x30  -- lda ($30),Y  -- Fetch element
        , 0xC8        -- iny
        , 0xD1, 0x30  -- cmp ($30,Y)  -- Is it larger than the next element?
        , 0x90, 0x10  -- bcc chkend
        , 0xF0, 0x0E  -- beq chkend
                                      -- It was, exchange elements in memory
        , 0x48        -- pha          -- By saving low byte on stack
        , 0xB1, 0x30  -- lda ($30),Y  -- then get high byte and
        , 0x88        -- dey          -- store it at low address
        , 0x91, 0x30  -- sta ($30),Y
        , 0x68        -- pla          -- Pull low byte from stack
        , 0xC8        -- iny          -- and store it at the high address
        , 0x91, 0x30  -- sta ($30),Y
        , 0xA9, 0xFF  -- lda #$FF     -- Turn exchange flag on (= -1)
        , 0x85, 0x32  -- sta $32
        ] :: [(Address, Operand)]

    chkend = zip [chkendadr..]
        [ 0xCA        -- dex          -- end of list?
        , 0xD0, 0xE6  -- bne nxtel    -- no, fetch next element
        , 0x24, 0x32  -- bit $32      -- yes, exchange flag still off?
        , 0x30, 0xD9  -- bmi sort8    -- no, go through list again
        , 0x60        -- rts          -- yes, list is now ordered
        ]

-- Bubblesort data stored at 0x6000 and onwards.
bubbleData :: [Int8] -> [(Address, Int8)]
bubbleData xs = [(0x30, 0x00) , (0x31, 0x60)]
             ++ (0x6000, fromIntegral $ length xs) : zip [0x6001..] xs
