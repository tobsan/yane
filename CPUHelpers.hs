module CPUHelpers
    ( setZN
    , branchIf

    -- * Stack operations
    , push, pushPC
    , pull, pullPC
    , fetch
    , hasCAdd, hasVAdd
    , hasCSub, hasVSub
    , compare'
    , mkAddr, (<#>)
    , alterSetC

    -- * Addressing modes.
    , zeropage, zeropageX, zeropageY
    , absolute, absoluteX, absoluteY
    , indirect, indirectX, indirectY
    , relative, immediate, implicit

    -- * IRQ
    , handleIRQ
    ) where

import Types
import CPUDefs

import Control.Monad (liftM2, when)
import Control.Applicative
import Data.Maybe (isJust)

-- import Debug.Trace
--
-- This module contains various handy functions
-- used when dealing with instructions
-- Note that no implementation of instructions goes here
--

(<+>) :: (Num a, Monad m) => m a -> m a -> m a
(<+>) = liftM2 (+)

-- | Push a operand on the stack
push :: (NesCPU m) => Operand -> m ()
push op = do res <- getSP
             setSP $ res-1
             writeMemory (baseSP + (fromIntegral res)) op

-- | Pull a operand from the stack
pull :: (NesCPU m) => m Operand
pull = alterSP (+1) >>= readMemory . (baseSP+) . fromIntegral

-- | Update CPU program counter if @b@ is satisfied and return if succeded.
branchIf :: (NesCPU m) => Address -> Bool -> m Bool
branchIf addr b = when b (setPC addr) >> return b

-- | Build a 16-bit address from low and high components.
mkAddr :: Operand -> Operand -> Address
mkAddr low high = ((transform high) `shiftL` 8) .|. (transform low)

-- checks if we have overflow in an addition
hasVAdd :: Operand -> Operand -> Operand -> Bool
hasVAdd op1 op2 res
    | op1 >= 0 && op2 >= 0 && res < 0   = True
    | op1 < 0  && op2 < 0  && res >= 0  = True
    | otherwise                         = False

-- checks if we have overflow in a subtraction
hasVSub :: Operand -> Operand -> Operand -> Bool
hasVSub op1 op2 res
    | op1 >= 0 && op2 <  0 && res <  0 = True
    | op1 <  0 && op2 >= 0 && res >= 0 = True
    | otherwise                        = False

-- checks if we have carry in an addition
hasCAdd :: Operand -> Operand -> Bool
hasCAdd op1 op2 = res > 0xFF
    where res = transform op1 + transform op2

-- checks if we have carry in an subtraction
hasCSub :: Operand -> Operand -> Bool
hasCSub op1 op2 = res < 0x100
    where res = transform op1 - transform op2

compare' :: (NesCPU m) => Operand -> Operand -> m ()
compare' mval reg = do
    setFlagC $ transform reg >= transform mval
    setFlagZ $ reg == mval
    setFlagN $ ((fromIntegral reg :: Int16) - fromIntegral mval) < 0

------------------------------------------------------------------------------
-- Functions used by the primitive functions.

-- Set Z and N flag.
setZN :: (NesCPU m) => Operand -> m ()
setZN oper = do setFlagZ $ oper == 0
                setFlagN $ oper < 0

-- Pull the program counter from stack
pullPC :: (NesCPU m) => m Address
pullPC = pull <#> pull

-- Push the program counter on stack
pushPC :: (NesCPU m) => m ()
pushPC = do res <- getPC
            push $ fromIntegral $ res `shiftR` 8
            push $ fromIntegral $ res

{-| Set C flag to current value of `m` at bit `b` and then alters `m` with `f`.
 -}
alterSetC  :: (NesCPU m)
           => Int
           -> ((Operand -> Operand) -> m Operand)
           -> (Operand -> Operand)
           -> m Operand
alterSetC b m f = do
    val <- m id
    setFlagC $ (val `testBit` b)
    m f

{-| Fetches the address PC currently points to, and updates PC
 -}
fetch :: (NesCPU m) => m OPCode
fetch = do
    setCPUAction NOP
    op <- getPC >>= readMemory
    alterPC (+1)
    return op

{-| `mkAddr` lifted as a binary operator.
 -}
(<#>) :: (NesCPU m) => m Operand -> m Operand -> m Address
(<#>) = liftM2 mkAddr

infixl 3 <#>

-- ---------------------------------------------------------------------------
-- Addressing modes.

{-| Fetch an 8 bit address operand from the stack
 -}
operand :: (NesCPU m) => m Operand
operand = fetch

{-| Fetch two 8 bit address operands from the stack and use them as a low 
 -  and high value of a 16 bit address.
 -}
address :: (NesCPU m) => m Address
address = fetch <#> fetch

{-| Zero page addressing uses only an 8 bit address operand to address the
 -  first 256 bytes in memory (e.g. $0000 to $00FF).
 -  Example: LDA $00 ; Load accumulator from $0000.
 -}
zeropage :: (NesCPU m) => m Address
zeropage  = transform <$> operand

{-| Indexed zero page addressing adds the value of register x to the value 
 -  of the 8 bit operand to address something in the first page.
 -  Example: STY $10,X ; Store the Y register at location $00(10+X).
 -}
zeropageX :: (NesCPU m) => m Address
zeropageX = zeropageIndex getX

{-| Indexed zero page addressing ...
 -}
zeropageY :: (NesCPU m) => m Address
zeropageY = zeropageIndex getY


zeropageIndex :: (NesCPU m) => m Operand -> m Address
-- zeropageIndex idx = transform <$> (idx <+> operand)
zeropageIndex idx = wToAddr <$> (opToW <$> idx) <+> (opToW <$> operand)
  where
    opToW = fromIntegral :: Operand -> Word8
    wToAddr = fromIntegral :: Word8 -> Address


{-| Fetch two bytes from stack and construct a absolute address.
 -}
absolute :: (NesCPU m) => m Address
absolute = address

{-| Fetches absolute address and adds register x.
 -}
absoluteX :: (NesCPU m) => m Address
absoluteX = absolute <+> (transform <$> getX)

{-| Fetches absolute address and adds register y.
 -}
absoluteY :: (NesCPU m) => m Address
absoluteY = absolute <+> (transform <$> getY)

{-| Indirect
 -}
indirect :: (NesCPU m) => m Address
indirect = do
    addr <- address
    -- offset bug in 6502, wraps on page boundary.
    let offset | (addr .&. 0x00FF) == 0xFF = addr - 0xFF
               | otherwise                 = addr + 1
    readMemory addr <#> readMemory offset

{-| Indexed Indirect
 -}
indirectX :: (NesCPU m) => m Address
indirectX = do
    addr <- zeropageX
    readMemory addr <#> readMemory (addr + 1)

{-| Indirect Indexed
 -}
indirectY :: (NesCPU m) => m Address
indirectY = do
    oper <- operand
    (rtfm oper <#> rtfm (oper + 1)) <+> (transform <$> getY)
  where rtfm = readMemory . transform

{-|
 -}
relative :: (NesCPU m) => m Address
relative = (fromIntegral <$> operand) <+> getPC

{-|
 -}
immediate :: (NesCPU m) => m Operand
immediate = operand

{-| 
 -}
implicit :: (NesCPU m) => m ()
implicit = return ()

-- | The interrupt handler.
handleIRQ :: (NesCPU m) => m ()
handleIRQ = do
    i <- getIRQ
    case i of
        Nothing  -> do
            return ()
        Just Normal -> do
            pushPC
            getStatus >>= push
            setFlagI True
            setFlagB False
            (readMemory 0xFFFE) <#> (readMemory 0xFFFF) >>= setPC
        Just NMI -> do
            pushPC
            getStatus >>= push
            (readMemory 0xFFFA) <#> (readMemory 0xFFFB) >>= setPC
        Just Reset -> do
            -- reset registers?
            (readMemory 0xFFFC) <#> (readMemory 0xFFFD) >>= setPC
    when (isJust i) $ setIRQ Nothing
