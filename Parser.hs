module Parser 
  ( assemble )
  where
    
  import Core
  
  data Instruction = 
    Instruction Opcode Operand Operand
  
  data Opcode
    = SET | ADD | SUB | MUL | DIV | MOD 
    | AND | BOR | XOR | IFE | IFN | IFG | IFB
    deriving (Show, Eq)
  
  data Operand 
    = AsmLiteral Int
    | AOperand
    | 
  
  assemble :: FilePath -> Either RAM 
  