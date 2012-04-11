{-# LANGUAGE OverloadedStrings #-}

module Parser 
  where
    
  import Core
  import Data.Attoparsec.ByteString (parseTest)
  import Data.Attoparsec.ByteString.Char8
  import Data.ByteString.Char8 (ByteString, pack)
  import qualified Data.ByteString.Char8 as B
  
  data Instruction = 
    Instruction Opcode Operand Operand
    deriving (Show, Eq)
  
  instruction :: Parser Instruction
  instruction = Instruction <$> opcode <*> operand <*> operand
  
  data Opcode
    = SET | ADD | SUB | MUL | DIV | MOD 
    | AND | BOR | XOR | IFE | IFN | IFG | IFB
    deriving (Show, Eq, Ord, Enum)
  
  opcode :: Parser Opcode
  opcode = valueParser [ SET .. IFB ]
  
  -- Since all the leaves of this ADT are different, maybe this would be a 
  -- good place to try coproducts?
  data Operand 
    = AsmLiteral Int
    | AsmRegister Register
    | AsmReference Operand
    | AsmLabel String
    deriving (Show, Eq)
  
  register :: Parser Operand
  register = AsmRegister <$> valueParser [ RA .. OF ]
  
  literal :: Parser Operand
  literal = AsmLiteral <$> ((string "0x" *> hexadecimal) <|> decimal)
  
  reference :: Parser Operand
  reference = AsmReference <$> (char '[' *> skipSpace *> operand <* skipSpace <* char ']')
  
  label, reference, literal, operand :: Parser Operand
  label = AsmLabel <$> identifier

  operand = skipSpace *> choice [ reference, register, label, literal ]
  
  assemble :: FilePath -> Either String RAM
  assemble = undefined
  
  identifier = undefined
  brackets = undefined
  
  skipComments :: Parser ()
  skipComments = char ';' *> skipWhile (/= '\n')
  
  program :: Parser [Instruction]
  program = some line
  
  line :: Parser Instruction
  line = skipSpace *> instruction <* skipComments <* (endOfLine <|> endOfInput)
  
  valueParser cs = choice $ do
    let tokens = pack <$> show <$> cs
    (value, token) <- zip cs tokens
    return $! stringCI token *> pure value
