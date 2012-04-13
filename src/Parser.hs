{-# LANGUAGE OverloadedStrings #-}

module Parser 
  where
    
  import Core
  import Data.Attoparsec.ByteString.Char8
  import Data.ByteString.Char8 (ByteString, pack)
  
  instruction :: Parser Instruction
  instruction = Instruction <$> opcode <*> (operand <* skipSpace <* char ',') <*> (skipSpace *> operand)
  
  opcode :: Parser Opcode
  opcode = valueParser [ SET .. IFB ]
  
  register, literal, reference, label, operand :: Parser Operand
  register  = AsmRegister   <$> valueParser [ RA .. OF ]
  literal   = AsmLiteral    <$> ((string "0x" *> hexadecimal) <|> decimal)
  reference = AsmReference  <$> (char '[' *> skipSpace *> operand <* skipSpace <* char ']')
  label     = AsmLabel      <$> identifier
  operand   = skipSpace      *> choice [ reference, register, label, literal ]
  
  assemble :: FilePath -> Either String RAM
  assemble = undefined
  
  identifier :: Parser ByteString
  identifier = takeWhile1 isAlpha_ascii
  
  skipComments :: Parser ()
  skipComments = char ';' *> skipWhile (/= '\n')
  
  program :: Parser [Instruction]
  program = some line
  
  line :: Parser Instruction
  line = skipSpace *> instruction <* skipComments <* (endOfLine <|> endOfInput)
  
  valueParser :: (Show a) => [a] -> Parser a
  valueParser cs = choice $ do
    let tokens = pack <$> show <$> cs
    (value, token) <- zip cs tokens
    return $! stringCI token *> pure value
