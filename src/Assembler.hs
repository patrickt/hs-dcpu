module Assembler where
  
  import Core hiding ((!))
  import Data.Bits
  import Data.ByteString
  import Data.Map (Map, (!))
  import Data.Maybe
  import qualified Data.Map as Map
  
  isLong :: Operand -> Bool
  isLong (AsmLabel _) = True -- TODO: optional short labels
  isLong (AsmReference _) = True
  isLong (AsmRegister _) = False
  isLong (AsmLiteral i) = i > 0x1f
  
  instructionLength :: Instruction -> Word
  instructionLength (Instruction _ _ a b) 
    | (isLong a) && (isLong b) = 3
    | (isLong a) `bxor` (isLong b) = 2
    | otherwise = 1
  
  textLength :: [Instruction] -> Word
  textLength x = fromIntegral $ sum $ instructionLength <$> x
  
  emptyText :: [Instruction] -> RAM
  emptyText x = emptyArray (0, textLength x)
  
  type LabelMap = Map ByteString Word
  
  intToWord :: Int -> Word
  intToWord n = fromInteger $ toInteger n
  
  registerToWord :: Register -> Word
  registerToWord RA = 0x0
  registerToWord RB = 0x1
  registerToWord RC = 0x2
  registerToWord RX = 0x3
  registerToWord RY = 0x4
  registerToWord RZ = 0x5
  registerToWord RI = 0x6
  registerToWord RJ = 0x7
  registerToWord SP = 0x1b
  registerToWord PC = 0x1c
  registerToWord OF = 0x1d
  
  compileOpcode :: Opcode -> Word
  compileOpcode o = fromIntegral $ toInteger $ fromEnum o
  
  data Chunk = Short Word | Long Word Word
  
  chunkAsExtraWord :: Chunk -> Maybe Word
  chunkAsExtraWord (Short _) = Nothing
  chunkAsExtraWord (Long _ l) = Just l
  
  compileOperand :: LabelMap -> Operand -> Chunk
  compileOperand _ (AsmRegister r) = Short $ registerToWord r
  compileOperand _ (AsmReference (AsmRegister r)) = Short $ 0x8 + registerToWord r
  compileOperand _ (AsmReference (AsmLiteral n)) = Long 0x1e $ intToWord n
  compileOperand _ (AsmLiteral n) 
    | n > 0x1f = Long 0x1f $ intToWord n
    | otherwise = Short $ intToWord n
  compileOperand lm (AsmLabel l) = Long 0x1f (lm ! l)
  compileOperand _ o = error x where x = ("BUG: unrecognized pattern" )
  
  nextWord :: Chunk
  nextWord = Short 0x1f
  
  glue :: Word -> Chunk -> Chunk -> Word
  glue op (Short a) (Short b) = (op .|. (a `shiftL` 4) .|. (b `shiftL` 10))
  glue op l@(Short _) (Long o _) = glue op l (Short o) 
  glue op (Long o _) r@(Short l) = glue op (Short o) r
  glue op (Long l _) (Long r _) = glue op (Short l) (Short r)
  
  doAssembly :: [Instruction] -> [Word]
  doAssembly x = x >>= \i -> do
    let labels = computeLabelMap x
    let opcode = compileOpcode $ op i
    let chunkA = compileOperand labels $ arga i
    let chunkB = compileOperand labels $ argb i 
    let firstWord = glue opcode chunkA chunkB
    let extraWords = catMaybes $ chunkAsExtraWord <$> [chunkA, chunkB]
    firstWord : extraWords
  
  computeLabelMap :: [Instruction] -> LabelMap
  computeLabelMap ins = go Map.empty ins 0 where
    go :: LabelMap -> [Instruction] -> Word -> LabelMap
    go m [] _ = m
    go m (i@(Instruction (Just label) _ _ _):rest) count = 
      go (Map.insert label count m) rest (count + (instructionLength i)) 
    go m (i@(Instruction Nothing _ _ _):rest) count = 
      go m rest (count + (instructionLength i)) 
  
  
  
  
  {-
  
  labelmap = {}
  count = 0
  with i = instruction[label, _, _, _] in ins:
    labelmap[label] = count
    count += instrLength i
    
  
  
  -}
  