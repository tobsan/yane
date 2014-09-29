module InstrSet where

import Types
import CPUDefs
import CPUHelpers

-- ********* GUIDELINES ***********
-- Insert the instructions you have written
-- in alphabetical order, include your name aswell.
--
-- Comment instructions with description and
-- Formal definition (like A,Z,N = M)

-- | Add with Carry
-- | A,Z,C,N = A+M+C
adc :: (NesCPU m) => Operand -> m ()
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
and :: (NesCPU m) => Operand -> m ()
and val = alterA (.&. val) >>= setZN

-- | Arithmetic Shift Left
-- | Z,C,N = M * 2
asl :: (NesCPU m) => Address -> m ()
asl addr = do
    alterSetC 7 (alterMemory addr) ((`clearBit` 0) . (`shiftL` 1)) >>= setZN

-- | Arithmetic Shift Left
-- | A,Z,C,N = A * 2
asla :: (NesCPU m) => m ()
asla = do alterSetC 7 alterA ((`clearBit` 0) . (`shiftL` 1)) >>= setZN

-- | Branch if carry is clear
bcc :: (NesCPU m) => Address -> m Bool
bcc addr = getFlagC >>= branchIf addr . not

-- | Branch if carry is set
bcs :: (NesCPU m) => Address -> m Bool
bcs addr = getFlagC >>= branchIf addr

-- | Branch if zero is set
beq :: (NesCPU m) => Address -> m Bool
beq addr = getFlagZ >>= branchIf addr

-- | Bit Test
-- | A & M, N = M7, V = M6
bit :: (NesCPU m) => Operand -> m ()
bit val = do acc <- getA
             setFlagV $ testBit val 6
             setFlagN $ testBit val 7
             setFlagZ $ (acc .&. val) == 0

-- | Branch if negative is set
bmi :: (NesCPU m) => Address -> m Bool
bmi addr = getFlagN >>= branchIf addr

-- | Branch if zero is clear
bne :: (NesCPU m) => Address -> m Bool
bne addr = getFlagZ >>= branchIf addr . not

-- | Branch if negative is clear (positive)
bpl :: (NesCPU m) => Address -> m Bool
bpl addr = getFlagN >>= branchIf addr . not

-- | Set the break flag, force interrupt
-- | B = 1
brk :: (NesCPU m) => m ()
brk = do
    alterPC succ
    pushPC
    setFlagB True
    getStatus >>= push
    setFlagI True
    low  <- readMemory 0xFFFE
    high <- readMemory 0xFFFF
    setPC (low `mkAddr` high)

-- | Branch if overflow is clear
bvc :: (NesCPU m) => Address -> m Bool
bvc addr = getFlagV >>= branchIf addr . not

-- | Branch if overflow is set
bvs :: (NesCPU m) => Address -> m Bool
bvs addr = getFlagV >>= branchIf addr

-- | Clear carry flag
-- | C = 0
clc :: (NesCPU m) => m ()
clc = setFlagC False

-- | Clear decimal mode flag
-- | D = 0
cld :: (NesCPU m) => m ()
cld = setFlagD False

-- | Clear interrupt disable flag
-- | I = 0
cli :: (NesCPU m) => m ()
cli = setFlagI False

-- | Clear overflow flag
-- | V = 0
clv :: (NesCPU m) => m ()
clv = setFlagV False

-- | Compare
-- | Z,C,N = A-M
cmp :: (NesCPU m) => Operand -> m ()
cmp mem = getA >>= compare' mem

-- | Compare X Reg
-- | Z,C,N = X-M
cpx :: (NesCPU m) => Operand -> m ()
cpx mem = getX >>= compare' mem

-- | Compare Y Reg
-- | Z,C,N = Y-M
cpy :: (NesCPU m) => Operand -> m ()
cpy mem = getY >>= compare' mem

-- | Decrement Mem
-- | M,Z,N = M-1
dec :: (NesCPU m) => Address -> m ()
dec addr = alterMemory addr (subtract 1) >>= setZN

-- Temp test!
deca :: (NesCPU m) => m ()
deca = alterA (subtract 1) >>= setZN

-- | Decrement X Reg
-- | X,Z,N = X-1
dex :: (NesCPU m) => m ()
dex = alterX (subtract 1) >>= setZN

-- | Decrement Y Reg
-- | Y,Z,N = Y-1
dey :: (NesCPU m) => m ()
dey = alterY (subtract 1) >>= setZN

-- | Exclusive OR
-- | A,Z,N = A^M
eor :: (NesCPU m) => Operand -> m ()
eor oper = alterA (`xor` oper) >>= setZN

-- | Increment Mem
-- | M,Z,N = M+1
inc :: (NesCPU m) => Address ->  m ()
inc addr = alterMemory addr (+1) >>= setZN

-- | Increment X Reg
-- | X,Z,N = X+1
inx :: (NesCPU m) => m ()
inx = alterX (+1) >>= setZN

-- | Increment Y Reg
-- | Y,Z,N = Y+1
iny :: (NesCPU m) => m ()
iny = alterY (+1) >>= setZN

-- | Jump to address
jmp :: (NesCPU m) => Address -> m ()
jmp = setPC

-- | Jump to subrutine
jsr :: (NesCPU m) => Address -> m ()
jsr addr = alterPC (subtract 1) >> pushPC >> setPC addr

-- | Load memory into accumulator
-- | A,Z,N = M
lda :: (NesCPU m) => Operand -> m ()
lda op = alterA (const op) >>= setZN

-- | Load memory into index x
-- | X,Z,N = M
ldx :: (NesCPU m) => Operand -> m ()
ldx op = alterX (const op) >>= setZN

-- | Load memory into index y
-- | Y,Z,N = M
ldy :: (NesCPU m) => Operand -> m ()
ldy op = alterY (const op) >>= setZN

-- | Logical Shift Right
-- | M,C,Z,N = M/2
lsr :: (NesCPU m) => Address -> m ()
lsr addr = do
    alterSetC 0 (alterMemory addr) ((`clearBit` 7) . (`shiftR` 1)) >>= setZN

-- | Logical Shift Right
-- | A,C,Z,N = A/2
lsra :: (NesCPU m) => m ()
lsra = do alterSetC 0 alterA ((`clearBit` 7) . (`shiftR` 1)) >>= setZN

-- | No operation
nop :: (NesCPU m) => m ()
nop = return ()

-- | Logical Inclusive OR
-- | A,Z,N = A|M
ora :: (NesCPU m) => Operand -> m ()
ora op = alterA (.|. op) >>= setZN

-- | Push a to stack
-- | M(SP) = A
pha :: (NesCPU m) => m ()
pha = getA >>= push

-- | Push status to stack
-- | M(SP) = P
php :: (NesCPU m) => m ()
php = setFlagB True >> getStatus >>= push

-- | Pull a from stack
-- | A,Z,N = M(SP)
pla :: (NesCPU m) => m ()
pla = pull >>= alterA . const >>= setZN

-- | Pull status from stack
-- | P = M(SP)
plp :: (NesCPU m) => m ()
plp = pull >>= setStatus

-- | Rotate Left
-- | Z,N,C,A
rola :: (NesCPU m) => m ()
rola = do
    setC <- getFlagC >>= \c -> if c then return (`setBit` 0) else return id
    alterSetC 7 alterA (setC . (`shiftL` 1)) >>= setZN

-- | Rotate Left
-- | Z,N,C,M
rol :: (NesCPU m) => Address -> m ()
rol addr =  do
    setC <- getFlagC >>= \c -> if c then return (`setBit` 0) else return id
    alterSetC 7 (alterMemory addr) (setC . (`shiftL` 1)) >>= setZN

-- | Rotate Right
-- | Z.N,C,A
rora :: (NesCPU m) => m ()
rora =  do
    setC <- getFlagC >>= \c -> if c then return (`setBit` 7) 
                                    else return (`clearBit` 7)
    alterSetC 0 alterA (setC . (`shiftR` 1)) >>= setZN

-- | Rotate Right
-- | Z,N,C,M
ror :: (NesCPU m) => Address -> m ()
ror addr =  do
    setC <- getFlagC >>= \c -> if c then return (`setBit` 7)
                                    else return (`clearBit` 7)
    alterSetC 0 (alterMemory addr) (setC . (`shiftR` 1)) >>= setZN

-- | Return from Interrupt
-- | C,Z,I,D,B,V,N,PC
rti :: (NesCPU m) => m ()
rti = do pull   >>= setStatus
         pullPC >>= setPC

-- | Return from subrutine
rts :: (NesCPU m) => m ()
rts = pullPC >>= setPC . (+1)

-- | Sub with Carry
-- | A,Z,C,N = A-M-(1-C)
sbc :: (NesCPU m) => Operand -> m ()
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
sec :: (NesCPU m) => m ()
sec = setFlagC True

-- | Set decimal mode flag
-- | D = 1
sed :: (NesCPU m) => m ()
sed = setFlagD True

-- | Set interrupt disable flag
-- | I = 1
sei :: (NesCPU m) => m ()
sei = setFlagI True

-- | Store accumulator in memory
-- | M = A
sta :: (NesCPU m) => Address -> m ()
sta addr = getA >>= writeMemory addr

-- | Store index register x in memory
-- | M = X
stx :: (NesCPU m) => Address -> m ()
stx addr = getX >>= writeMemory addr

-- | Store index register y in memory
-- | M = Y
sty :: (NesCPU m) => Address -> m ()
sty addr = getY >>= writeMemory addr

-- | Transfer A to X
-- | Z,N,X = A
tax :: (NesCPU m) => m ()
tax = getA >>= alterX . const >>= setZN

-- | Transfer A to Y
-- | Z,N,Y = A
tay :: (NesCPU m) => m ()
tay = getA >>= alterY . const >>= setZN

-- | Transfer SP to X
-- | Z,N,X = SP
tsx :: (NesCPU m) => m ()
tsx = getSP >>= alterX . const . fromIntegral >>= setZN

-- | Transfer X to A
-- | Z,N,A = X
txa :: (NesCPU m) => m ()
txa = getX >>= alterA . const >>= setZN

-- | Transfer X to SP
-- | SP = X
txs :: (NesCPU m) => m ()
txs = getX >>= setSP . fromIntegral

-- | Transfer Y to A
-- | Z,N,A = Y
tya :: (NesCPU m) => m ()
tya = getY >>= alterA . const >>= setZN
