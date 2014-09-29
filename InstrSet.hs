module InstrSet where

import Types
import CPUDefs
import CPUHelpers
-- import Debug.Trace

-- ********* GUIDELINES ***********
-- Insert the instructions you have written
-- in alphabetical order, include your name aswell.
--
-- Comment instructions with description and
-- Formal definition (like A,Z,N = M)

-- | Add with Carry
-- | A,Z,C,N = A+M+C
adc :: Operand -> CPU s ()
adc op = do
    acc  <- getA
    cFlag <- getFlagC
    let carry = (if cFlag then 1 else 0)
    let res = acc+op+carry
    setA res
    setZN res
    setFlagC $ hasCAdd acc (op+carry)
    setFlagV $ hasVAdd acc op res

-- | Logical AND
-- | A,Z,N = A&M
and :: Operand -> CPU s ()
and val = alterA (.&. val) >>= setZN

-- | Arithmetic Shift Left
-- | Z,C,N = M * 2
asl :: Address -> CPU s ()
asl addr = do
    alterSetC 7 (alterMem addr) ((`clearBit` 0) . (`shiftL` 1)) >>= setZN

-- | Arithmetic Shift Left
-- | A,Z,C,N = A * 2
asla :: CPU s ()
asla = do alterSetC 7 alterA ((`clearBit` 0) . (`shiftL` 1)) >>= setZN

-- | Branch if carry is clear
bcc :: Address -> CPU s Bool
bcc addr = getFlagC >>= branchIf addr . not

-- | Branch if carry is set
bcs :: Address -> CPU s Bool
bcs addr = getFlagC >>= branchIf addr

-- | Branch if zero is set
beq :: Address -> CPU s Bool
beq addr = getFlagZ >>= branchIf addr

-- | Bit Test
-- | A & M, N = M7, V = M6
bit :: Operand -> CPU s ()
bit val = do acc <- getA
             setFlagV $ testBit val 6
             setFlagN $ testBit val 7
             setFlagZ $ (acc .&. val) == 0

-- | Branch if negative is set
bmi :: Address -> CPU s Bool
bmi addr = getFlagN >>= branchIf addr

-- | Branch if zero is clear
bne :: Address -> CPU s Bool
bne addr = getFlagZ >>= branchIf addr . not

-- | Branch if negative is clear (positive)
bpl :: Address -> CPU s Bool
bpl addr = getFlagN >>= branchIf addr . not

-- | Set the break flag, force interrupt
-- | B = 1
brk :: CPU s ()
brk = do
    alterPC succ
    pushPC
    setFlagB True
    getStatus >>= push
    setFlagI True
    low  <- readMemory 0xFFFE
    high <- readMemory 0xFFFF
    alterPC (const $ mkAddr low high) >> return ()

-- | Branch if overflow is clear
bvc :: Address -> CPU s Bool
bvc addr = getFlagV >>= branchIf addr . not

-- | Branch if overflow is set
bvs :: Address -> CPU s Bool
bvs addr = getFlagV >>= branchIf addr

-- | Clear carry flag
-- | C = 0
clc :: CPU s ()
clc = setFlagC False

-- | Clear decimal mode flag
-- | D = 0
cld :: CPU s ()
cld = setFlagD False

-- | Clear interrupt disable flag
-- | I = 0
cli :: CPU s ()
cli = setFlagI False

-- | Clear overflow flag
-- | V = 0
clv :: CPU s ()
clv = setFlagV False

-- | Compare
-- | Z,C,N = A-M
cmp :: Operand -> CPU s ()
cmp mem = getA >>= compare' mem

-- | Compare X Reg
-- | Z,C,N = X-M
cpx :: Operand -> CPU s ()
cpx mem = getX >>= compare' mem

-- | Compare Y Reg
-- | Z,C,N = Y-M
cpy :: Operand -> CPU s ()
cpy mem = getY >>= compare' mem

-- | Decrement Mem
-- | M,Z,N = M-1
dec :: Address -> CPU s ()
dec addr = alterMem addr (subtract 1) >>= setZN

-- Temp test!
deca :: CPU s ()
deca = alterA (subtract 1) >>= setZN

-- | Decrement X Reg
-- | X,Z,N = X-1
dex :: CPU s ()
dex = alterX (subtract 1) >>= setZN

-- | Decrement Y Reg
-- | Y,Z,N = Y-1
dey :: CPU s ()
dey = alterY (subtract 1) >>= setZN

-- | Exclusive OR
-- | A,Z,N = A^M
eor :: Operand -> CPU s ()
eor oper = alterA (`xor` oper) >>= setZN

-- | Increment Mem
-- | M,Z,N = M+1
inc :: Address ->  CPU s ()
inc addr = alterMem addr (+1) >>= setZN

-- | Increment X Reg
-- | X,Z,N = X+1
inx :: CPU s ()
inx = alterX (+1) >>= setZN

-- | Increment Y Reg
-- | Y,Z,N = Y+1
iny :: CPU s ()
iny = alterY (+1) >>= setZN

-- | Jump to address
jmp :: Address -> CPU s ()
jmp = setPC

-- | Jump to subrutine
jsr :: Address -> CPU s ()
jsr addr = alterPC (subtract 1) >> pushPC >> setPC addr

-- | Load memory into accumulator
-- | A,Z,N = M
lda :: Operand -> CPU s ()
lda op = alterA (const op) >>= setZN --applyTo oper [setA, setZN]

-- | Load memory into index x
-- | X,Z,N = M
ldx :: Operand -> CPU s ()
ldx op = alterX (const op) >>= setZN

-- | Load memory into index y
-- | Y,Z,N = M
ldy :: Operand -> CPU s ()
ldy op = alterY (const op) >>= setZN

-- | Logical Shift Right
-- | M,C,Z,N = M/2
lsr :: Address -> CPU s ()
lsr addr = do
    alterSetC 0 (alterMem addr) ((`clearBit` 7) . (`shiftR` 1)) >>= setZN

-- | Logical Shift Right
-- | A,C,Z,N = A/2
lsra :: CPU s ()
lsra = do alterSetC 0 alterA ((`clearBit` 7) . (`shiftR` 1)) >>= setZN

-- | No operation
nop :: CPU s ()
nop = return ()

-- | Logical Inclusive OR
-- | A,Z,N = A|M
ora :: Operand -> CPU s ()
ora op = alterA (.|. op) >>= setZN

-- | Push a to stack
-- | M(SP) = A
pha :: CPU s ()
pha = getA >>= push

-- | Push status to stack
-- | M(SP) = P
php :: CPU s ()
php = setFlagB True >> getStatus >>= push

-- | Pull a from stack
-- | A,Z,N = M(SP)
pla :: CPU s ()
pla = pull >>= alterA . const >>= setZN

-- | Pull status from stack
-- | P = M(SP)
plp :: CPU s ()
plp = pull >>= setStatus

-- | Rotate Left
-- | Z,N,C,A
rola :: CPU s ()
rola = do
    setC <- getFlagC >>= \c -> if c then return (`setBit` 0) else return id
    alterSetC 7 alterA (setC . (`shiftL` 1)) >>= setZN

-- | Rotate Left
-- | Z,N,C,M
rol :: Address -> CPU s ()
rol addr =  do
    setC <- getFlagC >>= \c -> if c then return (`setBit` 0) else return id
    alterSetC 7 (alterMem addr) (setC . (`shiftL` 1)) >>= setZN

-- | Rotate Right
-- | Z.N,C,A
rora :: CPU s ()
rora =  do
    setC <- getFlagC >>= \c -> if c then return (`setBit` 7) 
                                    else return (`clearBit` 7)
    alterSetC 0 alterA (setC . (`shiftR` 1)) >>= setZN

-- | Rotate Right
-- | Z,N,C,M
ror :: Address -> CPU s ()
ror addr =  do
    setC <- getFlagC >>= \c -> if c then return (`setBit` 7)
                                    else return (`clearBit` 7)
    alterSetC 0 (alterMem addr) (setC . (`shiftR` 1)) >>= setZN

-- | Return from Interrupt
-- | C,Z,I,D,B,V,N,PC
rti :: CPU s ()
rti = do pull   >>= setStatus
         pullPC >>= setPC

-- | Return from subrutine
rts :: CPU s ()
rts = pullPC >>= setPC . (+1)

-- | Sub with Carry
-- | A,Z,C,N = A-M-(1-C)
sbc :: Operand -> CPU s ()
sbc op = do
    flagC <- getFlagC
    acc <- getA
    let carry = (if flagC then 0 else 1)
    let res = acc - (op+carry)
    setA     $ res
    setZN    $ res
    setFlagC $ hasCSub acc (op+carry)
    setFlagV $ hasVSub acc op res

-- | Set carry flag
-- | C = 1
sec :: CPU s ()
sec = setFlagC True

-- | Set decimal mode flag
-- | D = 1
sed :: CPU s ()
sed = setFlagD True

-- | Set interrupt disable flag
-- | I = 1
-- | By Tobsan
sei :: CPU s ()
sei = setFlagI True

-- | Store accumulator in memory
-- | M = A
sta :: Address -> CPU s ()
sta addr = getA >>= writeMemory addr

-- | Store index register x in memory
-- | M = X
stx :: Address -> CPU s ()
stx addr = getX >>= writeMemory addr

-- | Store index register y in memory
-- | M = Y
sty :: Address -> CPU s ()
sty addr = getY >>= writeMemory addr

-- | Transfer A to X
-- | Z,N,X = A
tax :: CPU s ()
tax = getA >>= alterX . const >>= setZN

-- | Transfer A to Y
-- | Z,N,Y = A
tay :: CPU s ()
tay = getA >>= alterY . const >>= setZN

-- | Transfer SP to X
-- | Z,N,X = SP
tsx :: CPU s ()
tsx = getSP >>= alterX . const . fromIntegral >>= setZN

-- | Transfer X to A
-- | Z,N,A = X
txa :: CPU s ()
txa = getX >>= alterA . const >>= setZN

-- | Transfer X to SP
-- | SP = X
txs :: CPU s ()
txs = getX >>= setSP . fromIntegral

-- | Transfer Y to A
-- | Z,N,A = Y
tya :: CPU s ()
tya = getY >>= alterA . const >>= setZN
