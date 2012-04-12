{-# LANGUAGE OverloadedStrings #-}

module Main where

  import Core
  import Parser
  
  import Data.Attoparsec hiding (Result)
  import Data.ByteString.Char8 (pack)
  import Test.Framework
  import Test.Framework.Providers.QuickCheck2 (testProperty)
  import Test.QuickCheck
  import Text.Printf
  import Prelude hiding ((++))
  
  instance Arbitrary Register  where
    arbitrary = elements [ minBound .. maxBound ]
    
  
  instance Arbitrary Opcode where
    arbitrary = elements [ minBound .. maxBound ]  
  
  (?==) :: (Eq a, Show a) => a -> a -> Property
  a ?== b 
    | a == b = property True
    | otherwise = fail $ printf "Expected `%s', got `%s'" (show a) (show b)
  
  parserTests :: [Test]
  parserTests = [ testProperty "decimal constants" prop_integers
                , testProperty "hexadecimal constants" prop_hexIntegers 
                , testProperty "registers" prop_registers
                , testProperty "opcodes" prop_opcodes
                ]
  
  prop_hexIntegers :: Property
  prop_hexIntegers = do 
    (NonNegative input) <- arbitrary
    let hex = pack $ printf "0x%x" input
    case parseOnly literal hex of
      Right (AsmLiteral n) -> n ?== input
      Right d -> fail $ printf "Unexpected result: `%s`" $ show d
      Left s -> fail $ printf "Parse error: %s" s
  
  prop_integers :: Property
  prop_integers = do
    (NonNegative input) <- arbitrary
    case parseOnly literal $ bshow input of
      Right (AsmLiteral n) -> n ?== input
      Right d -> fail $ printf "Unexpected result: `%s`" $ show d
      Left s -> fail $ printf "Parse error: %s" s
  
  prop_registers :: Property
  prop_registers = prop_roundtrip register AsmRegister
  
  prop_opcodes :: Property
  prop_opcodes = prop_roundtrip opcode id
  
  prop_roundtrip p c = do 
    r <- arbitrary
    case parseOnly p $ bshow r of
      Right r' -> c r ?== r'
      Left s  -> fail $ printf "Parse error: %s" s
  
  tests :: [Test]
  tests = [ testGroup "parsing" parserTests ]
    
  main :: IO ()
  main = defaultMain tests
  