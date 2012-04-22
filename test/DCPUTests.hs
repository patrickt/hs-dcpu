{-# LANGUAGE OverloadedStrings #-}

module Main where

  import Core
  import Parser
  
  import Data.ByteString.Char8 (pack)
  import Test.Framework
  import Test.Framework.Providers.QuickCheck2 (testProperty)
  import Test.QuickCheck
  import Text.Parsec
  import Text.Printf
  import Prelude hiding ((++))
  
  instance Arbitrary ByteString where
    arbitrary = pack <$> arbitrary
  
  instance Arbitrary Register  where
    arbitrary = elements [ minBound .. maxBound ]
  
  instance Arbitrary Opcode where
    arbitrary = elements [ minBound .. maxBound ]
  
  instance Arbitrary Operand where
    arbitrary = oneof [ AsmRegister <$> arbitrary 
                      , AsmReference <$> arbitrary ]
  
  instance Arbitrary Instruction where 
    arbitrary = Instruction <$> arbitrary <*> arbitrary <*> arbitrary
  
  (?==) :: (Eq a, Show a) => a -> a -> Property
  a ?== b 
    | a == b = property True
    | otherwise = fail $ printf "Expected `%s', got `%s'" (show a) (show b)
  
  
  parserTests :: [Test]
  parserTests = [ testProperty "instructions" prop_instructions ]
    
    {-[ testProperty "decimal constants" prop_integers
                , testProperty "hexadecimal constants" prop_hexIntegers 
                , testProperty "registers" prop_registers
                , testProperty "opcodes" prop_opcodes
                , testProperty "operands" prop_operands
                ]
                
  prop_hexIntegers :: Property
  prop_hexIntegers = do 
    (NonNegative input) <- arbitrary
    let hex = pack $ printf "0x%x" input
    case runParser operand hex empty of
      Right (AsmLiteral n) -> n ?== input
      Right d -> fail $ printf "Unexpected result: `%s`" $ show d
      Left s -> fail $ printf "Parse error: %s" s
  
  prop_integers :: Property
  prop_integers = do
    (NonNegative input) <- arbitrary
    case runParser operand $ bshow input $ empty of
      Right (AsmLiteral n) -> n ?== input
      Right d -> fail $ printf "Unexpected result: `%s`" $ show d
      Left s -> fail $ printf "Parse error: %s" s
  
  prop_operands :: Property
  prop_operands = prop_roundtrip operand id
  
  prop_registers :: Property
  prop_registers = do
    r <- arbitrary :: Gen Register
    case parse register $ bshow r of
      Right (AsmRegister r') -> r ?== r'
      Left s -> fail $ printf "Parse error: %s" s
  
  prop_opcodes :: Property
  prop_opcodes = prop_roundtrip opcode id
  
  -}
  
  prop_instructions :: Property
  prop_instructions = prop_roundtrip instruction
  
  prop_roundtrip p = do 
    r <- arbitrary
    case parse p empty $ bshow r of
      Right r' -> r ?== r'
      Left s  -> fail $ printf "Parse error: %s" $ show s
      
  tests :: [Test]
  tests = [ testGroup "parsing" parserTests ]
    
  main :: IO ()
  main = defaultMain tests
  