{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.Bits
import Data.Word hiding (Word)
import Debug.Trace
import Text.ParserCombinators.Parsec
import Text.PrettyPrint hiding (ptext)
import Text.Printf

import Core
import Pretty

-- the CPU has RAM and registers 
data CPU = CPU 
  { ram  :: RAM
  , regs :: Slots
  , ptext :: RAM
  }

instance Pretty CPU where
  pretty (CPU ram regs ptext) = vcat [pretty ram, pretty regs]

-- this should really be an Arrow
runCPU :: DCPU a -> (a, CPU)
runCPU a = runState a mkCPU

mkCPU :: CPU
mkCPU = CPU 
  { ram  = emptyArray (0, 0x100)
  , regs = emptyArray (RA, OF)
  , ptext = emptyArray (0, 0x100)
  } where emptyArray minmax = array minmax [ (i, 0) | i <- range minmax ]

-- Values are the arguments to CPU operations. They can be literal values,
-- register variables, or references.
data Value 
  = Imm Word
  | Reg Register
  | Ref Value
  deriving (Show, Eq)
  
readValue :: Value -> DCPU Word
readValue (Imm a) = return a
readValue (Reg r) = getRegister r
readValue (Ref (Imm a)) = readMemory a
readValue (Ref (Reg r)) = getRegister r >>= readMemory
readValue (Ref r) = readValue r >>= readMemory

writeValue :: Value -> Word -> DCPU ()
writeValue (Reg destreg) val = setRegister destreg val
writeValue (Ref r) val = do { r' <- readValue r; writeMemory r' val }
writeValue (Imm loc) val =  writeMemory loc val


-- A CPU is stateful. Stateful computations run inside the DCPU monad.
type DCPU a = StateT CPU Identity a


step ::  DCPU ()
step = do
  pc <- getRegister PC
  setRegister PC (pc + 1)
  
-- Reading/writing values to/from memory and registers.

getRegister :: Register -> DCPU Word
getRegister reg = do
  r <- get >>= return . regs
  return (r ! reg)

setRegister :: Register -> Word -> DCPU ()
setRegister reg val = do 
  cpu <- get
  let slots = regs cpu
  let updated = slots // [(reg, val)]
  put $ cpu { regs = updated }

readMemory :: Word -> DCPU Word
readMemory i = do
  mem <- get >>= return . ram
  return (mem ! i)
  
writeMemory :: Word -> Word -> DCPU ()
writeMemory loc val = do
  cpu <- get
  let mem = ram cpu
  let newMem = mem // [(loc, val)]
  put $ cpu { ram = newMem }

-- Basic instructions.

type BasicOp = Value -> Value -> DCPU ()

-- Takes an operation and two operands, readValueuates their value, and writes 
-- overwrites the left operand with the result.
-- TODO: implement overflow.
liftOp :: (Word -> Word -> Word) -> BasicOp
liftOp op l r = case l of
  (Imm _) -> error "bad lvalue"
  otherwise -> do
    left <- readValue l
    right <- readValue r
    write l (op left right)
    where 
      write (Reg destreg) val = setRegister destreg val
      write (Imm loc) val = writeMemory loc val
      write (Ref r) val = write r val

{-
overOp :: (Word -> Word -> Word) -> (Word32 -> Bool) -> (Word32 -> DCPU ()) -> BasicOp
overOp op test over l r =
  left <- readValue l
  right <- readValue r
  let result = op left right :: Word32
  when (test result) (over result)
  write res
  



-}
-- Point-free style. There is no need to be upset.
addOp, setOp, mulOp, divOp, modOp, shlOp,
  andOp, borOp, xorOp :: BasicOp

addOp l r = do
  left <- readValue l
  right <- readValue r
  let sum = fromIntegral $ left + right :: Word32
  if sum > (fromIntegral $ (maxBound :: Word16) :: Word32) then
    setRegister OF 0x1
  else
    setRegister OF 0x0
  writeValue l (fromIntegral sum)

--addOp = liftOp (+)
setOp = liftOp (flip const)
--subOp = liftOp (-)
subOp l r = do 
  left <- readValue l
  right <- readValue r
  let diff = fromIntegral $ left - right :: Word32
  if diff > (fromIntegral $(minBound :: Word16) :: Word32) then
    setRegister OF 0xFFFF
  else
    setRegister OF 0x0
  writeValue l (fromIntegral (left - right) :: Word16)

mulOp = liftOp (*)
divOp = liftOp div
modOp = liftOp mod
shlOp = liftOp shiftL'
shrOp = liftOp shiftR'
andOp = liftOp (.&.)
borOp = liftOp (.|.)
xorOp = liftOp xor

shiftL' a b = shiftL a (fromIntegral b)
shiftR' a b = shiftR a (fromIntegral b)

-- Bit-twiddling
-- 
-- decodeOpcode :: Word -> DCPU ()
-- decodeOpcode :: 
-- decodeOpcode w = do
--   let op = opcodeToOp w
--   let val1 = opcodeToVal 10
--   

condOp :: (Word -> Word -> Bool) -> BasicOp
condOp op l r = do
  left <- readValue l
  right <- readValue r
  unless (op left right) $ do
    
    pc <- getRegister PC;
    setRegister PC (pc + 1)

ifeOp :: BasicOp
ifeOp = condOp (==)
ifnOp = condOp (/=)
ifgOp = condOp (>)
ifbOp = condOp (\a b -> (a .&. b) /= 0)

-- Testing

test :: DCPU Word

test = do
  
  actionFromOpcode 0xfc01 -- add A 0x1f
  -- 
  -- setOp (Reg RA) (Reg RB)
  -- setOp (Ref (Imm 0x10)) (Imm 0x20)
  -- subOp (Reg RA) (Ref (Imm 0x10))
  -- setOp (Ref (Imm 0x11)) (Imm 0x65534)
  -- addOp (Ref (Imm 0x11)) (Imm 0x50)
  getRegister RA

  
actionFromOpcode :: Word -> DCPU ()
actionFromOpcode w = 
  let 
  op = opcodeToOp w
  val1 = valueFromOpcode w 4
  val2 = valueFromOpcode w 10
  in op val1 val2
  
  
valueFromOpcode :: Word -> Int -> Value
valueFromOpcode w len = 
   interpret ((w `shiftR` len) .&. 0x3f)
  where
    interpret b 
      | elem b [0x00..0x07] = Reg (toEnum (fromIntegral b))
      | elem b [0x08..0x0f] = Ref (Reg (toEnum ((fromIntegral (b - 8)) :: Int)))
      | elem b [0x20..0x3f] = Imm (b - 0x20)
      | otherwise = error $ show b

opcodeToOp :: Word -> BasicOp
opcodeToOp n = case n .&. 0x000f of
  0x0 -> undefined
  0x1 -> setOp
  0x2 -> addOp 
  0x3 -> subOp
  0x4 -> mulOp
  0x5 -> divOp
  0x6 -> modOp
  0x7 -> shlOp
  0x8 -> shrOp
  0x9 -> andOp
  0xa -> borOp 
  0xb -> xorOp

main = print $ pretty $ snd $ runCPU test