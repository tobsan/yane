module CPU where

import Prelude hiding (and)
import Control.Monad (liftM2)
import CPUDefs
import Instr
import InstrSet

-- | Fetches the address PC currently points to, and updates PC
fetch :: NES s OPCode
fetch = gets pc >>= readMemory >>= \op -> alterPC (+1) >> return op

-- | Executes an instruction
execute :: OPCode -> NES s Int
execute op = case op of
    -- ADC
    0x69 -> oper        >>= adc >> return 2--immediate
    0x65 -> zeropage    >>= readMemory >>= adc >> return 3
    0x75 -> zeropageX   >>= readMemory >>= adc >> return 4
    0x6D -> addr        >>= readMemory >>= adc >> return 4 --absolute
    0x7D -> absoluteX   >>= readMemory >>= adc >> return 4
    0x79 -> absoluteY   >>= readMemory >>= adc >> return 4
    0x61 -> indirectX   >>= readMemory >>= adc >> return 6
    0x71 -> indirectY   >>= readMemory >>= adc >> return 5
    -- AND
    0x29 -> oper        >>= and >> return 2 --immediate
    0x25 -> zeropage    >>= readMemory >>= and >> return 3
    0x35 -> zeropageX   >>= readMemory >>= and >> return 4
    0x2D -> addr        >>= readMemory >>= and >> return 4 --absolute
    0x3D -> absoluteX   >>= readMemory >>= and >> return 4
    0x39 -> absoluteY   >>= readMemory >>= and >> return 4
    0x21 -> indirectX   >>= readMemory >>= and >> return 6
    0x31 -> indirectY   >>= readMemory >>= and >> return 5
    -- ASL
    0x0A -> asla >> return 2 --special case - accumulator
    0x06 -> zeropage    >>= asl >> return 5
    0x16 -> zeropageX   >>= asl >> return 6
    0x0E -> addr        >>= asl >> return 6 --absolute
    0x1E -> absoluteX   >>= asl >> return 7
    -- Branches
    0x90 -> branch >>= bcc >>= \b -> return $ if b then 3 else 2
    0xB0 -> branch >>= bcs >>= \b -> return $ if b then 3 else 2
    0xF0 -> branch >>= beq >>= \b -> return $ if b then 3 else 2
    --BIT
    0x24 -> zeropage    >>= readMemory >>= bit >> return 3
    0x2C -> addr        >>= readMemory >>= bit >> return 4 --absolute
    -- More branches
    0x30 -> branch >>= bmi >>= \b -> return $ if b then 3 else 2
    0xD0 -> branch >>= bne >>= \b -> return $ if b then 3 else 2
    0x10 -> branch >>= bpl >>= \b -> return $ if b then 3 else 2
    0x00 -> brk >> return 7
    -- More branches
    0x50 -> branch >>= bvc >>= \b -> return $ if b then 3 else 2
    0x70 -> branch >>= bvs >>= \b -> return $ if b then 3 else 2
    -- Flag clearing
    0x18 -> clc >> return 2
    0xD8 -> cld >> return 2
    0x58 -> cli >> return 2
    0xB8 -> clv >> return 2
    --CMP
    0xC9 -> oper        >>= cmp >> return 2 --immediate
    0xC5 -> zeropage    >>= readMemory >>= cmp >> return 3
    0xD5 -> zeropageX   >>= readMemory >>= cmp >> return 4
    0xCD -> addr        >>= readMemory >>= cmp >> return 4 --absolute
    0xDD -> absoluteX   >>= readMemory >>= cmp >> return 4
    0xD9 -> absoluteY   >>= readMemory >>= cmp >> return 4
    0xC1 -> indirectX   >>= readMemory >>= cmp >> return 6
    0xD1 -> indirectY   >>= readMemory >>= cmp >> return 5
    --CPX
    0xE0 -> oper        >>= cpx >> return 2 --immediate
    0xE4 -> zeropage    >>= readMemory >>= cpx >> return 3
    0xEC -> addr        >>= readMemory >>= cpx >> return 4 --absolute
    --CPY
    0xC0 -> oper        >>= cpy >> return 2 --immediate
    0xC4 -> zeropage    >>= readMemory >>= cpy >> return 3
    0xCC -> addr        >>= readMemory >>= cpy >> return 4 --absolute
    --DEC
    0xC6 -> zeropage    >>= dec >> return 5
    0xD6 -> zeropageX   >>= dec >> return 6
    0xCE -> addr        >>= dec >> return 6 --absolute
    0xDE -> absoluteX   >>= dec >> return 7
    -- Other decs
    0x3A -> deca >> return 2
    0xCA -> dex >> return 2
    0x88 -> dey >> return 2
    --EOR
    0x49 -> oper        >>= eor >> return 2 --immediate
    0x45 -> zeropage    >>= readMemory >>= eor >> return 3
    0x55 -> zeropageX   >>= readMemory >>= eor >> return 4
    0x4D -> addr        >>= readMemory >>= eor >> return 4 --absolute
    0x5D -> absoluteX   >>= readMemory >>= eor >> return 4
    0x59 -> absoluteY   >>= readMemory >>= eor >> return 4
    0x41 -> indirectX   >>= readMemory >>= eor >> return 6
    0x51 -> indirectY   >>= readMemory >>= eor >> return 5
    --INC
    0xE6 -> zeropage    >>= inc >> return 5
    0xF6 -> zeropageX   >>= inc >> return 6
    0xEE -> addr        >>= inc >> return 6 --absolute
    0xFE -> absoluteX   >>= inc >> return 7
    --Other incs
    0xE8 -> inx >> return 2
    0xC8 -> iny >> return 2
    --JMP
    0x4C -> addr        >>= jmp >> return 3 --absolute
    0x6C -> indirect    >>= jmp >> return 5
    --JSR
    0x20 -> addr >>= jsr >> return 6
    --LDA
    0xA9 -> oper        >>= lda >> return 2 --immediate
    0xA5 -> zeropage    >>= readMemory >>= lda >> return 3
    0xB5 -> zeropageX   >>= readMemory >>= lda >> return 4
    0xAD -> addr        >>= readMemory >>= lda >> return 4 --absolute
    0xBD -> absoluteX   >>= readMemory >>= lda >> return 4
    0xB9 -> absoluteY   >>= readMemory >>= lda >> return 4
    0xA1 -> indirectX   >>= readMemory >>= lda >> return 6
    0xB1 -> indirectY   >>= readMemory >>= lda >> return 5
    --LDX
    0xA2 -> oper        >>= ldx >> return 2 --immediate
    0xA6 -> zeropage    >>= readMemory >>= ldx >> return 3
    0xB6 -> zeropageX   >>= readMemory >>= ldx >> return 4
    0xAE -> addr        >>= readMemory >>= ldx >> return 4 --absolute
    0xBE -> absoluteY   >>= readMemory >>= ldx >> return 4
    --LDY
    0xA0 -> oper        >>= ldy >> return 2 --immediate
    0xA4 -> zeropage    >>= readMemory >>= ldy >> return 3
    0xB4 -> zeropageX   >>= readMemory >>= ldy >> return 4
    0xAC -> addr        >>= readMemory >>= ldy >> return 4 --absolute
    0xBC -> absoluteX   >>= readMemory >>= ldy >> return 4
    --LSR
    0x4A -> lsra >> return 2
    0x46 -> zeropage    >>= lsr >> return 5
    0x56 -> zeropageX   >>= lsr >> return 6
    0x4E -> addr        >>= lsr >> return 6
    0x5E -> absoluteX   >>= lsr >> return 7
    --NOP
    0xEA -> nop >> return 2
    --ORA
    0x09 -> oper        >>= ora >> return 2 --immediate
    0x05 -> zeropage    >>= readMemory >>= ora >> return 3
    0x15 -> zeropageX   >>= readMemory >>= ora >> return 4
    0x0D -> addr        >>= readMemory >>= ora >> return 4 --absolute
    0x1D -> absoluteX   >>= readMemory >>= ora >> return 4
    0x19 -> absoluteY   >>= readMemory >>= ora >> return 4
    0x01 -> indirectX   >>= readMemory >>= ora >> return 6
    0x11 -> indirectY   >>= readMemory >>= ora >> return 5
    -- Stack stuff
    0x48 -> pha >> return 3
    0x08 -> php >> return 3
    0x68 -> pla >> return 4
    0x28 -> plp >> return 4
    --ROL
    0x2A -> rola >> return 2
    0x26 -> zeropage    >>= rol >> return 5
    0x36 -> zeropageX   >>= rol >> return 6
    0x2E -> addr        >>= rol >> return 6
    0x3E -> absoluteX   >>= rol >> return 7
    --ROR
    0x6A -> rora >> return 2
    0x66 -> zeropage    >>= ror >> return 5
    0x76 -> zeropageX   >>= ror >> return 6
    0x6E -> addr        >>= ror >> return 6
    0x7E -> absoluteX   >>= ror >> return 7
    --RTI and RTS
    0x40 -> rti >> return 6
    0x60 -> rts >> return 6
    --SBC
    0xE9 -> oper        >>= sbc  >> return 2
    0xE5 -> zeropage    >>= readMemory >>= sbc >> return 3
    0xF5 -> zeropageX   >>= readMemory >>= sbc >> return 4
    0xED -> addr        >>= readMemory >>= sbc >> return 4 --absolute
    0xFD -> absoluteX   >>= readMemory >>= sbc >> return 4
    0xF9 -> absoluteY   >>= readMemory >>= sbc >> return 4
    0xE1 -> indirectX   >>= readMemory >>= sbc >> return 6
    0xF1 -> indirectY   >>= readMemory >>= sbc >> return 5
    -- Flag setting
    0x38 -> sec >> return 2
    0xF8 -> sed >> return 2
    0x78 -> sei >> return 2
    --STA
    0x85 -> zeropage    >>= sta >> return 3
    0x95 -> zeropageX   >>= sta >> return 4
    0x8D -> addr        >>= sta >> return 4 --absolute
    0x9D -> absoluteX   >>= sta >> return 5
    0x99 -> absoluteY   >>= sta >> return 5
    0x81 -> indirectX   >>= sta >> return 6
    0x91 -> indirectY   >>= sta >> return 6
    --STX
    0x86 -> zeropage    >>= stx >> return 3
    0x96 -> zeropageY   >>= stx >> return 4
    0x8E -> addr        >>= stx >> return 4 --absolute
    --STY
    0x84 -> zeropage    >>= sty >> return 3
    0x94 -> zeropageX   >>= sty >> return 4
    0x8C -> addr        >>= sty >> return 4 --absolute
    --Transfers
    0xAA -> tax >> return 2
    0xA8 -> tay >> return 2
    0xBA -> tsx >> return 2
    0x8A -> txa >> return 2
    0x9A -> txs >> return 2
    0x98 -> tya >> return 2
    -- Otherwise return 0, indicating that something went wrong, or that
    -- the program has ended (memory is just garbage). 1 would make more sense here
    _ -> return 0
  where
    oper = fetch
    addr = liftM2 mkAddr fetch fetch
    relative    = oper >>= getAddress . Relative
    zeropage    = oper >>= getAddress . ZeroPage Nothing
    zeropageX   = oper >>= getAddress . ZeroPage (Just X)
    zeropageY   = oper >>= getAddress . ZeroPage (Just Y)
    absoluteX   = addr >>= getAddress . Absolute (Just X)
    absoluteY   = addr >>= getAddress . Absolute (Just Y)
    indirect    = addr >>= getAddress . Indirect
    indirectX   = oper >>= getAddress . Indexed Before
    indirectY   = oper >>= getAddress . Indexed After
    branch      = oper >>= getAddress . Relative
