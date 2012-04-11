{-# LANGUAGE OverloadedStrings #-}

module Parser 
  where
    
  import Core
  import Data.Attoparsec.ByteString.Char8
  import Data.Attoparsec.Types hiding (Parser)
  
  
  data Instruction = 
    Instruction Opcode Operand Operand
    deriving (Show, Eq)
  
  data Opcode
    = SET | ADD | SUB | MUL | DIV | MOD 
    | AND | BOR | XOR | IFE | IFN | IFG | IFB
    deriving (Show, Eq, Ord, Enum)
  
  -- Since all the leaves of this ADT are different, maybe this would be a 
  -- good place to try coproducts?
  data Operand 
    = AsmLiteral Int
    | AsmRegister Register
    | AsmReference Operand
    | AsmLabel String
    deriving (Show, Eq)
  
  label, register, reference, literal, operand :: Parser Operand
  label = AsmLabel <$> identifier
  register = AsmRegister <$> choice [ stringCI token *> pure reg | (token, reg) <- zip tokens regs]
    where 
      tokens = [ "A", "B", "C", "X", "Y", "Z", "I", "J", "O", "PC", "SP"]
      regs   = [ RA, RB, RC, RX, RY, RZ, RI, RJ, OF, PC, SP]
  
  reference = AsmReference <$> (char '[' *> skipSpace *> operand <* skipSpace <* char ']')
  literal = AsmLiteral <$> (string "0x" *> hexadecimal)
  operand = skipSpace *> choice [ reference, register, label, literal ]
  
  instruction :: Parser Instruction
  instruction = Instruction <$> opcode <*> operand <*> operand
  
  assemble :: FilePath -> Either String RAM
  assemble = undefined
  
  opcode :: Parser Opcode
  opcode = choice 
    [ s "set" *> pure SET
    , s "add" *> pure ADD ] where s = stringCI
  
  identifier = undefined
  brackets = undefined
  
  skipComments :: Parser ()
  skipComments = char ';' *> skipWhile (/= '\n')
  
  program :: Parser [Instruction]
  program = some line
  
  line :: Parser Instruction
  line = skipSpace *> instruction <* skipComments <* (endOfLine <|> endOfInput)
  