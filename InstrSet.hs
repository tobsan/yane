module InstrSet where

import Instr
import CPUDefs
import Control.Monad.State.Strict
import Data.Bits

-- ********* GUIDELINES ***********
-- Insert the instructions you have written
-- in alphabetical order, include your name aswell.
--
-- Comment instructions with description and
-- Formal definition (like A,Z,N = M)

-- | Add with Carry
-- | A,Z,C,N = A+M+C
-- | By David
adc :: Operand -> NES ()
adc op = do
    acc     <- gets aReg
    stat    <- gets status
    let carry = if c stat then 1 else 0
    let res = acc+op+carry
    modify $ \cpu -> cpu { aReg = res, status = (setZN res stat) {
        c = hasCAdd acc (op+carry),
        v = hasVAdd acc op res
    }}

-- | Logical AND
-- | A,Z,N = A&M
-- | by David
and :: Operand -> NES ()
and val = do
    cpu <- get
    let res = aReg cpu .&. val
    put cpu {aReg = res, status = setZN res (status cpu) }

-- | Arithmetic Shift Left
-- | Z,C,N = M * 2
-- | By David
asl :: Address -> NES ()
asl mem =  do
    val <- readMemory mem
    let res = clearBit (shiftL val 1) 0
    alterStatus $ \st -> (setZN res st) { c = testBit val 7 }
    writeMemory mem res

-- | Arithmetic Shift Left
-- | A,Z,C,N = A * 2
-- | By David
asla :: NES ()
asla = do
    acc     <- gets aReg
    stat    <- gets status
    let res = clearBit (shiftL acc 1) 1
    modify $ \cpu -> cpu {aReg = res,
                     status = (setZN res stat) {c = testBit acc 7}}

-- | Branch if carry is clear, returns if branching sucedeee
-- | By trez
bcc :: Address -> NES Bool
bcc = branchIf $ not . c

-- | Branch if carry is set
-- | By trez
bcs :: Address -> NES Bool
bcs = branchIf $ c

-- | Branch if zero is set
-- | By trez
beq :: Address -> NES Bool
beq = branchIf $ z

-- | Bit Test
-- | A & M, N = M7, V = M6
-- | By David
bit :: Operand -> NES ()
bit val = do
    acc <- gets aReg
    alterStatus $ \st -> st { v = testBit val 6
                            , n = testBit val 7
                            , z = (acc .&. val) == 0 }

-- | Branch if negative is set
-- | By trez
bmi :: Address -> NES Bool
bmi = branchIf $ n

-- | Branch if zero is clear
-- | By trez
bne :: Address -> NES Bool
bne = branchIf $ not . z

-- | Branch if negative is clear (positive)
-- | By trez
bpl :: Address -> NES Bool
bpl = branchIf $ not . n

-- | Set the break flag, force interrupt
-- | B = 1
-- | By Tobsan
brk :: NES ()
brk = do
    pushPC
    alterStatus $ \st -> st { b = True }
    gets status >>= push . toOper
    -- at this point brk should signal an interrupt
    low  <- readMemory 0xFFFE
    high <- readMemory 0xFFFF
    alterPC (const $ mkAddr low high)

-- | Branch if overflow is clear
-- | By trez
bvc :: Address -> NES Bool
bvc = branchIf $ not . v

-- | Branch if overflow is set
-- | By trez
bvs :: Address -> NES Bool
bvs = branchIf $ v

-- | Clear carry flag
-- | C = 0
-- | By Tobsan
clc :: NES ()
clc = alterStatus $ \st -> st { c = False }

-- | Clear decimal mode flag
-- | D = 0
-- | By Tobsan
cld :: NES ()
cld = alterStatus $ \st -> st { d = False }

-- | Clear interrupt disable flag
-- | I = 0
-- | By Tobsan
cli :: NES ()
cli = alterStatus $ \st -> st { i = False }

-- | Clear overflow flag
-- | V = 0
-- | By Tobsan
clv :: NES ()
clv = alterStatus $ \st -> st { v = False }

-- | Compare
-- | Z,C,N = A-M
-- | By David
cmp :: Operand -> NES ()
cmp mem = modify $ \cpu -> cpu {status = compare' (aReg cpu) mem (status cpu)}

-- | Compare X Reg
-- | Z,C,N = X-M
-- | By David
cpx :: Operand -> NES ()
cpx mem = modify $ \cpu -> cpu {status = compare' (xReg cpu) mem (status cpu)}

-- | Compare Y Reg
-- | Z,C,N = Y-M
-- | By David
cpy :: Operand -> NES ()
cpy mem = modify $ \cpu -> cpu {status = compare' (yReg cpu) mem (status cpu)}

-- | Decrement Mem
-- | M,Z,N = M-1
-- | By David
dec :: Address -> NES ()
dec addr = do
    val <- readMemory addr
    let res = val - 1
    writeMemory addr res
    alterStatus $ setZN res

-- Temp test!
deca :: NES ()
deca = do
    cpu <- get
    let a = aReg cpu - 1
    put cpu { aReg = a, status = setZN a (status cpu) }

-- | Decrement X Reg
-- | X,Z,N = X-1
-- | By David
dex :: NES ()
dex = do
    cpu <- get
    let x = xReg cpu - 1
    put cpu { xReg = x, status = setZN x (status cpu) }

-- | Decrement Y Reg
-- | Y,Z,N = Y-1
-- | By David
dey :: NES ()
dey = do
    cpu <- get
    let y = yReg cpu - 1
    put cpu {yReg = y, status = setZN y (status cpu) }

-- | Exclusive OR
-- | A,Z,N = A^M
-- | By David
eor :: Operand -> NES ()
eor mem = do
    a <- gets aReg
    let res = a `xor` mem
    modify $ \cpu -> cpu {aReg = res, status = setZN res $ status cpu}

-- | Increment Mem
-- | M,Z,N = M+1
-- | By David
inc :: Address ->  NES ()
inc addr = do
    val <- readMemory addr
    let res = val + 1
    writeMemory addr res
    alterStatus $ setZN res

-- | Increment X Reg
-- | X,Z,N = X+1
-- | By David
inx :: NES ()
inx = modify $ \cpu -> cpu {
    xReg = (xReg cpu) + 1,
    status = setZN (xReg cpu +1) $ status cpu }

-- | Increment Y Reg
-- | Y,Z,N = Y+1
-- | By David
iny :: NES ()
iny = modify $ \cpu -> cpu {
    yReg = (yReg cpu) + 1,
    status = setZN (yReg cpu +1) $ status cpu }

-- | Jump to address
-- | By trez
jmp :: Address -> NES ()
jmp = alterPC . const

-- | Jump to subrutine
-- | By trez
jsr :: Address -> NES ()
jsr addr = alterPC (subtract 1) >> pushPC >> jmp addr

-- | Load memory into accumulator
-- | A,Z,N = M
-- | By Tobsan
lda :: Operand -> NES ()
lda oper = modify $ \cpu -> cpu {
    aReg = oper,
    status = setZN oper $ status cpu}

-- | Load memory into index x
-- | X,Z,N = M
-- | By Tobsan
ldx :: Operand -> NES ()
ldx oper = modify $ \cpu -> cpu {
    xReg = oper,
    status = setZN oper $ status cpu}

-- | Load memory into index y
-- | Y,Z,N = M
-- | By Tobsan
ldy :: Operand -> NES ()
ldy oper = modify $ \cpu -> cpu {
    yReg = oper,
    status = setZN oper $ status cpu }

-- | Logical Shift Right
-- | M,C,Z,N = M/2
-- | By David
lsr :: Address -> NES ()
lsr mem = do
    val <- readMemory mem
    let res = clearBit (shiftR val 1) 7
    alterStatus $ \st -> (setZN res st) { c = testBit val 0 }
    writeMemory mem res

-- | Logical Shift Right
-- | A,C,Z,N = A/2
-- | By David edit by Tobsan
lsra :: NES ()
lsra = do
    op <- gets aReg
    let res = clearBit (shiftR op 1) 7
    modify $ \cpu -> cpu {
        aReg = res,
        status = setZN res (status cpu) { c = testBit op 0 }}

-- | No operation
-- | By Tobsan
nop :: NES ()
nop = return ()

-- | Logical Inclusive OR
-- | A,Z,N = A|M
-- | By David, edit by Tobsan
ora :: Operand -> NES ()
ora mem = do
    cpu <- get
    let res = aReg cpu .|. mem
    put cpu {aReg = res, status = setZN res $ status cpu}

-- | Push a to stack
-- | M(SP) = A
-- | By Tobsan
pha :: NES ()
pha = get >>= push . aReg

-- | Push status to stack
-- | M(SP) = P
-- | By Tobsan
php :: NES ()
php =  do --get >>= push . toOper . status
    alterStatus $ \st -> st { b = True } -- Bug: "Feature"
    stat <- gets status
    push $ toOper $ stat

-- | Pull a from stack
-- | A,Z,N = M(SP)
-- | By Tobsan
pla :: NES ()
pla = do
    val <- pull
    modify $ \cpu -> cpu { aReg = val
                         , status = setZN val (status cpu) }

-- | Pull status from stack
-- | P = M(SP)
-- | By Tobsan
plp :: NES ()
plp = pull >>= alterStatus . const . fromOper

-- | Rotate Left
-- | Z,N,C,A
-- | By David, edited by Tobsan
rola :: NES ()
rola = do
    status  <- gets status
    op      <- gets aReg
    let res = if (c status) then setBit (shiftL op 1) 0 else shiftL op 1
    modify $ \cpu -> cpu {
        aReg    = res,
        status  = setZN res status { c = testBit op 7 }
    }

-- | Rotate Left
-- | Z,N,C,M
-- | By David edited by Tobsan
rol :: Address -> NES ()
rol mem =  do
    status  <- gets status
    val     <- readMemory mem
    let res = if c status then setBit (shiftL val 1) 0 else shiftL val 1
    alterStatus $ \st -> (setZN res st) { c = testBit val 7 }
    writeMemory mem res

-- | Rotate Right
-- | Z.N,C,A
-- | By David edited by Tobsan
rora :: NES ()
rora =  do
    status <- gets status
    op <- gets aReg
    let res' = shiftR op 1
    let res = if c status then setBit res' 7 else clearBit res' 7
    modify $ \cpu -> cpu {
        aReg = res,
        status = setZN res status { c = testBit op 0 }
    }

-- | Rotate Right
-- | Z,N,C,M
-- | By David edited by Tobsan
ror :: Address -> NES ()
ror mem =  do
    val <- readMemory mem
    status <- gets status
    let res' = shiftR val 1
    let res = if c status then setBit res' 7 else clearBit res' 7
    alterStatus $ \st -> (setZN res st) { c = testBit val 0 }
    writeMemory mem res

-- | Return from Interrupt
-- | C,Z,I,D,B,V,N,PC
-- | By trez
rti :: NES ()
rti = do
    cpu <- get
    status <- pull
    addr   <- pullPC
    put cpu { status = fromOper status, pc = addr }

-- | Return to subrutine
-- | By trez
rts :: NES ()
rts = pullPC >>= alterPC . const . (+1)

-- | Sub with Carry
-- | A,Z,C,N = A-M-(1-C)
-- | By David edited by Tobsan
sbc :: Operand -> NES ()
sbc op = do
    stat    <- gets status
    acc     <- gets aReg
    let carry = if c stat then 0 else 1
    let res = acc - (op+carry)
    modify $ \cpu -> cpu {aReg = res,
        status = (setZN res stat) {
            c = hasCSub acc (op+carry),
            v = hasVSub acc op res
        }
    }

-- | Set carry flag
-- | C = 1
-- | By Tobsan
sec :: NES ()
sec = alterStatus $ \st -> st { c = True }

-- | Set decimal mode flag
-- | D = 1
-- | By Tobsan
sed :: NES ()
sed = alterStatus $ \st -> st { d = True }

-- | Set interrupt disable flag
-- | I = 1
-- | By Tobsan
sei :: NES ()
sei = alterStatus $ \st -> st { i = True }

-- | Store accumulator in memory
-- | M = A
-- | By Tobsan
sta :: Address -> NES ()
sta addr = get >>= writeMemory addr . aReg

-- | Store index register x in memory
-- | M = X
-- | By Tobsan
stx :: Address -> NES ()
stx addr = get >>= writeMemory addr . xReg

-- | Store index register y in memory
-- | M = Y
-- | By Tobsan
sty :: Address -> NES ()
sty addr = get >>= writeMemory addr . yReg

-- | Transfer A to X
-- | Z,N,X = A
-- | By Tobsan
tax :: NES ()
tax = modify $ \cpu -> cpu { xReg = (aReg cpu),
                             status = setZN (aReg cpu) (status cpu) }

-- | Transfer A to Y
-- | Z,NY = A
-- | By Tobsan
tay :: NES ()
tay = modify $ \cpu -> cpu { yReg = (aReg cpu),
                             status = setZN (aReg cpu) (status cpu) }

-- | Transfer SP to X
-- | Z,N,X = SP
-- | By Tobsan
tsx :: NES ()
tsx = modify $ \cpu -> cpu { xReg = (fromIntegral $ sp cpu),
                             status = setZN (fromIntegral $ sp cpu) (status cpu) }

-- | Transfer X to A
-- | Z,N,A = X
-- | By Tobsan
txa :: NES ()
txa = modify $ \cpu -> cpu { aReg = (xReg cpu),
                             status = setZN (xReg cpu) (status cpu) }

-- | Transfer X to SP
-- | SP = X
-- | By Tobsan
txs :: NES ()
txs = modify $ \cpu -> cpu { sp = fromIntegral (xReg cpu) }

-- | Transfer Y to A
-- | Z,N,A = Y
-- | By Tobsan
tya :: NES ()
tya = modify $ \cpu -> cpu { aReg = yReg cpu,
                             status = setZN (yReg cpu) (status cpu) }
