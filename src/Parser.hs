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
    | AsmRegister Register
    | AsmReference Operand
    | AsmLabel String
  
  label, register, reference, literal, operand :: Parser Operand
  label = AsmLabel <$> identifier
  register = AsmRegister <$> reg
  reference = AsmReference <$> brackets $ operand
  literal = AsmLiteral <$> hexInt
  operand = choice [ reference, register, label, literal ]
  
  opcode :: Parser Opcode
  
  instruction :: Parser Instruction
  instruction = Instruction <$> opcode <*> operand <*> operand 
  
  assemble :: FilePath -> Either String RAM
  assemble = undefined
  
  identifier = undefined
  brackets = undefined
  