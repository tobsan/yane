module CPU where

import Prelude hiding (and)
import Types
import Control.Monad (liftM2)
import CPUDefs
import CPUHelpers
import InstrSet

-- | Fetches the address PC currently points to, and updates PC
fetch :: CPU s OPCode
fetch = do 
    setAction NOP
    op <- getPC >>= readMemory
    alterPC (+1)
    return op

-- | Executes an instruction
execute :: OPCode -> CPU s Int
execute op = case op of
    -- ADC
    0x69 -> immediate   >>= adc >> return 2
    0x65 -> zeropage    >>= readMemory >>= adc >> return 3
    0x75 -> zeropageX   >>= readMemory >>= adc >> return 4
    0x6D -> absolute    >>= readMemory >>= adc >> return 4
    0x7D -> absoluteX   >>= readMemory >>= adc >> return 4
    0x79 -> absoluteY   >>= readMemory >>= adc >> return 4
    0x61 -> indirectX   >>= readMemory >>= adc >> return 6
    0x71 -> indirectY   >>= readMemory >>= adc >> return 5
    -- AND
    0x29 -> immediate   >>= and >> return 2
    0x25 -> zeropage    >>= readMemory >>= and >> return 3
    0x35 -> zeropageX   >>= readMemory >>= and >> return 4
    0x2D -> absolute    >>= readMemory >>= and >> return 4
    0x3D -> absoluteX   >>= readMemory >>= and >> return 4
    0x39 -> absoluteY   >>= readMemory >>= and >> return 4
    0x21 -> indirectX   >>= readMemory >>= and >> return 6
    0x31 -> indirectY   >>= readMemory >>= and >> return 5
    -- ASL
    0x0A -> implicit    >>  asla >> return 2
    0x06 -> zeropage    >>= asl  >> return 5
    0x16 -> zeropageX   >>= asl  >> return 6
    0x0E -> absolute    >>= asl  >> return 6
    0x1E -> absoluteX   >>= asl  >> return 7
    -- Branches
    0x90 -> relative >>= bcc >>= \b -> return $ if b then 3 else 2
    0xB0 -> relative >>= bcs >>= \b -> return $ if b then 3 else 2
    0xF0 -> relative >>= beq >>= \b -> return $ if b then 3 else 2
    --BIT
    0x24 -> zeropage    >>= readMemory >>= bit >> return 3
    0x2C -> absolute    >>= readMemory >>= bit >> return 4
    -- More relativees
    0x30 -> relative >>= bmi >>= \b -> return $ if b then 3 else 2
    0xD0 -> relative >>= bne >>= \b -> return $ if b then 3 else 2
    0x10 -> relative >>= bpl >>= \b -> return $ if b then 3 else 2
    -- Break
    0x00 -> implicit >> brk >> return 7
    -- More relativees
    0x50 -> relative >>= bvc >>= \b -> return $ if b then 3 else 2
    0x70 -> relative >>= bvs >>= \b -> return $ if b then 3 else 2
    -- Flag clearing
    0x18 -> implicit >> clc >> return 2
    0xD8 -> implicit >> cld >> return 2
    0x58 -> implicit >> cli >> return 2
    0xB8 -> implicit >> clv >> return 2
    --CMP
    0xC9 -> immediate   >>= cmp >> return 2
    0xC5 -> zeropage    >>= readMemory >>= cmp >> return 3
    0xD5 -> zeropageX   >>= readMemory >>= cmp >> return 4
    0xCD -> absolute    >>= readMemory >>= cmp >> return 4
    0xDD -> absoluteX   >>= readMemory >>= cmp >> return 4
    0xD9 -> absoluteY   >>= readMemory >>= cmp >> return 4
    0xC1 -> indirectX   >>= readMemory >>= cmp >> return 6
    0xD1 -> indirectY   >>= readMemory >>= cmp >> return 5
    --CPX
    0xE0 -> immediate   >>= cpx >> return 2
    0xE4 -> zeropage    >>= readMemory >>= cpx >> return 3
    0xEC -> absolute    >>= readMemory >>= cpx >> return 4
    --CPY
    0xC0 -> immediate   >>= cpy >> return 2
    0xC4 -> zeropage    >>= readMemory >>= cpy >> return 3
    0xCC -> absolute    >>= readMemory >>= cpy >> return 4
    --DEC
    0xC6 -> zeropage    >>= dec >> return 5
    0xD6 -> zeropageX   >>= dec >> return 6
    0xCE -> absolute    >>= dec >> return 6
    0xDE -> absoluteX   >>= dec >> return 7
    -- Other decs
    0x3A -> implicit    >>  deca >> return 2
    0xCA -> implicit    >>  dex  >> return 2
    0x88 -> implicit    >>  dey  >> return 2
    --EOR
    0x49 -> immediate   >>= eor >> return 2
    0x45 -> zeropage    >>= readMemory >>= eor >> return 3
    0x55 -> zeropageX   >>= readMemory >>= eor >> return 4
    0x4D -> absolute    >>= readMemory >>= eor >> return 4
    0x5D -> absoluteX   >>= readMemory >>= eor >> return 4
    0x59 -> absoluteY   >>= readMemory >>= eor >> return 4
    0x41 -> indirectX   >>= readMemory >>= eor >> return 6
    0x51 -> indirectY   >>= readMemory >>= eor >> return 5
    --INC
    0xE6 -> zeropage    >>= inc >> return 5
    0xF6 -> zeropageX   >>= inc >> return 6
    0xEE -> absolute    >>= inc >> return 6
    0xFE -> absoluteX   >>= inc >> return 7
    --Other incs
    0xE8 -> implicit    >>  inx >> return 2
    0xC8 -> implicit    >>  iny >> return 2
    --JMP
    0x4C -> absolute    >>= jmp >> return 3
    0x6C -> indirect    >>= jmp >> return 5
    --JSR
    0x20 -> absolute >>= jsr >> return 6
    --LDA
    0xA9 -> immediate   >>= lda >> return 2
    0xA5 -> zeropage    >>= readMemory >>= lda >> return 3
    0xB5 -> zeropageX   >>= readMemory >>= lda >> return 4
    0xAD -> absolute    >>= readMemory >>= lda >> return 4
    0xBD -> absoluteX   >>= readMemory >>= lda >> return 4
    0xB9 -> absoluteY   >>= readMemory >>= lda >> return 4
    0xA1 -> indirectX   >>= readMemory >>= lda >> return 6
    0xB1 -> indirectY   >>= readMemory >>= lda >> return 5
    --LDX
    0xA2 -> immediate   >>= ldx >> return 2
    0xA6 -> zeropage    >>= readMemory >>= ldx >> return 3
    0xB6 -> zeropageX   >>= readMemory >>= ldx >> return 4
    0xAE -> absolute    >>= readMemory >>= ldx >> return 4
    0xBE -> absoluteY   >>= readMemory >>= ldx >> return 4
    --LDY
    0xA0 -> immediate   >>= ldy >> return 2
    0xA4 -> zeropage    >>= readMemory >>= ldy >> return 3
    0xB4 -> zeropageX   >>= readMemory >>= ldy >> return 4
    0xAC -> absolute    >>= readMemory >>= ldy >> return 4
    0xBC -> absoluteX   >>= readMemory >>= ldy >> return 4
    --LSR
    0x4A -> implicit    >>  lsra >> return 2
    0x46 -> zeropage    >>= lsr  >> return 5
    0x56 -> zeropageX   >>= lsr  >> return 6
    0x4E -> absolute    >>= lsr  >> return 6
    0x5E -> absoluteX   >>= lsr  >> return 7
    --NOP
    0xEA -> implicit    >>  nop  >> return 2
    --ORA
    0x09 -> immediate   >>= ora >> return 2
    0x05 -> zeropage    >>= readMemory >>= ora >> return 3
    0x15 -> zeropageX   >>= readMemory >>= ora >> return 4
    0x0D -> absolute    >>= readMemory >>= ora >> return 4
    0x1D -> absoluteX   >>= readMemory >>= ora >> return 4
    0x19 -> absoluteY   >>= readMemory >>= ora >> return 4
    0x01 -> indirectX   >>= readMemory >>= ora >> return 6
    0x11 -> indirectY   >>= readMemory >>= ora >> return 5
    -- Stack stuff
    0x48 -> implicit    >>  pha >> return 3
    0x08 -> implicit    >>  php >> return 3
    0x68 -> implicit    >>  pla >> return 4
    0x28 -> implicit    >>  plp >> return 4
    --ROL
    0x2A -> implicit    >>  rola >> return 2
    0x26 -> zeropage    >>= rol  >> return 5
    0x36 -> zeropageX   >>= rol  >> return 6
    0x2E -> absolute    >>= rol  >> return 6
    0x3E -> absoluteX   >>= rol  >> return 7
    --ROR
    0x6A -> implicit    >>  rora >> return 2
    0x66 -> zeropage    >>= ror  >> return 5
    0x76 -> zeropageX   >>= ror  >> return 6
    0x6E -> absolute    >>= ror  >> return 6
    0x7E -> absoluteX   >>= ror  >> return 7
    --RTI and RTS
    0x40 -> implicit    >>  rti >> return 6
    0x60 -> implicit    >>  rts >> return 6
    --SBC
    0xE9 -> immediate   >>= sbc >> return 2
    0xE5 -> zeropage    >>= readMemory >>= sbc >> return 3
    0xF5 -> zeropageX   >>= readMemory >>= sbc >> return 4
    0xED -> absolute    >>= readMemory >>= sbc >> return 4
    0xFD -> absoluteX   >>= readMemory >>= sbc >> return 4
    0xF9 -> absoluteY   >>= readMemory >>= sbc >> return 4
    0xE1 -> indirectX   >>= readMemory >>= sbc >> return 6
    0xF1 -> indirectY   >>= readMemory >>= sbc >> return 5
    -- Flag setting
    0x38 -> implicit    >> sec >> return 2
    0xF8 -> implicit    >> sed >> return 2
    0x78 -> implicit    >> sei >> return 2
    --STA
    0x85 -> zeropage    >>= sta >> return 3
    0x95 -> zeropageX   >>= sta >> return 4
    0x8D -> absolute    >>= sta >> return 4
    0x9D -> absoluteX   >>= sta >> return 5
    0x99 -> absoluteY   >>= sta >> return 5
    0x81 -> indirectX   >>= sta >> return 6
    0x91 -> indirectY   >>= sta >> return 6
    --STX
    0x86 -> zeropage    >>= stx >> return 3
    0x96 -> zeropageY   >>= stx >> return 4
    0x8E -> absolute    >>= stx >> return 4
    --STY
    0x84 -> zeropage    >>= sty >> return 3
    0x94 -> zeropageX   >>= sty >> return 4
    0x8C -> absolute    >>= sty >> return 4
    --Transfers
    0xAA -> implicit    >> tax >> return 2
    0xA8 -> implicit    >> tay >> return 2
    0xBA -> implicit    >> tsx >> return 2
    0x8A -> implicit    >> txa >> return 2
    0x9A -> implicit    >> txs >> return 2
    0x98 -> implicit    >> tya >> return 2
    -- Otherwise return 0, indicating that something went wrong, or that
    -- the program has ended (memory is just garbage). 1 would make more sense here
    -- It might be a good idea to throw an exception at this point?
    _ -> return 0
  where
    oper = fetch
    addr = liftM2 mkAddr fetch fetch
    implicit    = return ()
    immediate   = oper
    absolute    = addr
    relative    = oper >>= getAddress . Relative
    zeropage    = oper >>= getAddress . ZeroPage Nothing
    zeropageX   = oper >>= getAddress . ZeroPage (Just X)
    zeropageY   = oper >>= getAddress . ZeroPage (Just Y)
    absoluteX   = addr >>= getAddress . Absolute (Just X)
    absoluteY   = addr >>= getAddress . Absolute (Just Y)
    indirect    = addr >>= getAddress . Indirect
    indirectX   = oper >>= getAddress . Indexed Before
    indirectY   = oper >>= getAddress . Indexed After
