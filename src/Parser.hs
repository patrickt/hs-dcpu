{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

module Parser 
  where
    
  import Core hiding (label)
  import Data.ByteString.Char8 (ByteString)
  import qualified Data.ByteString.Char8 as B
  import Text.Parsec hiding ((<|>), optional, parse, label)
  import qualified Text.Parsec as P
  import Text.Parsec.ByteString
  import qualified Text.Parsec.Token as T
  import Text.Parsec.Language (emptyDef)
  import GHC.Exts
  
  type LanguageDef m = T.GenLanguageDef ByteString () m
  
  dcpuAsm :: (Monad m) => LanguageDef m
  dcpuAsm = T.LanguageDef { 
    T.commentStart = empty,
    T.commentEnd = empty,
    T.commentLine = ";",
    T.nestedComments = False,
    T.identStart = letter <|> char '_',
    T.identLetter = alphaNum <|> char '_',
    T.opStart = mzero,
    T.opLetter = mzero,
    T.reservedNames = show <$> [ SET .. IFB ],
    T.reservedOpNames = mzero,
    T.caseSensitive = False
  }
  
  parse = P.parse
  
  lexer = T.makeTokenParser dcpuAsm
  natural = T.natural lexer
  ident = pack <$> T.identifier lexer
  brackets = T.brackets lexer
  colon = T.colon lexer
  comma = T.comma lexer
  lexeme = T.lexeme lexer
  whitespace = T.whiteSpace lexer
  reserved = T.reserved lexer

  instruction :: Parser Instruction
  instruction = Instruction <$> label <*> (opcode <* whitespace) <*> operand <*> (comma *> operand) <?> "instruction"

  label :: Parser (Maybe ByteString)
  label = optional (ident <* colon)

  opcode :: Parser Opcode
  opcode = valueParser [ SET .. IFB ] <?> "opcode"
  
  register :: Parser Register
  register = valueParser [ RA .. OF ] 
  
  operand :: Parser Operand
  operand = choice 
    [ AsmRegister <$> register <?> "register"
    , AsmLiteral <$> fromInteger <$> natural <?> "number"
    , AsmLabel <$> ident <?> "label"
    , AsmReference <$> brackets operand <?> "reference"
    ]
  
  program :: Parser [Instruction]
  program = sepBy1 instruction whitespace
  
  valueParser :: (Show a) => [a] -> Parser a
  valueParser cs = choice $ do
    let ts = map show cs
    (value, token) <- zip ts cs
    return $ reserved value *> pure token
    