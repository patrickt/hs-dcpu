module Assembler where
  
  import Core
  import Data.Bits
  import Data.STRef
  
  isLong :: Operand -> Bool
  isLong (AsmLabel _) = True -- TODO: optional short labels
  isLong (AsmReference _) = True
  isLong (AsmRegister _) = False
  isLong (AsmLiteral i) = i > 0x1f
  
  instructionLength :: Instruction -> Int
  instructionLength (Instruction _ _ a b) 
    | (isLong a) && (isLong b) = 3
    | (isLong a) `bxor` (isLong b) = 2
    | otherwise = 1
  
  textLength :: [Instruction] -> Word
  textLength x = fromIntegral $ sum $ instructionLength <$> x
  
  emptyText :: [Instruction] -> RAM
  emptyText x = emptyArray (0, textLength x)
  
  -- compileOperand :: Operand -> (Word, Maybe Word)
  -- compileOperand (AsmRegister r) = (compileRegister r, Nothing)
  -- compileOperand (AsmReference (AsmRegister r)) = (0x8 + compileRegister r, Nothing)
  -- compileOperand (AsmReference (Asm ))
  -- -- compileOperand (AsmLabel )
  
  compileRegister :: Register -> Word
  compileRegister RA = 0x0
  compileRegister RB = 0x1
  compileRegister RC = 0x2
  compileRegister RX = 0x3
  compileRegister RY = 0x4
  compileRegister RZ = 0x5
  compileRegister RI = 0x6
  compileRegister RJ = 0x7
  compileRegister SP = 0x1b
  compileRegister PC = 0x1c
  compileRegister OF = 0x1d
  
  -- too dumb to figure out how to do this manually
  doAssembly :: [Instruction] -> ST a RAM
  doAssembly x = do
    text <- newSTRef $ emptyText x
    let labels = label <$> x
    return undefined -- ??
