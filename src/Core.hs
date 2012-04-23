module Core 
  ( Word
  , RAM 
  , Register (..)
  , Slots
  , Instruction (..)
  , Opcode (..)
  , Operand (..)
  , module Control.Applicative
  , module Control.Monad
  , module Data.Array.Unboxed
  , module Data.Monoid
  , module Data.Word
  , module Data.ByteString.Char8
  , (<>)
  , bshow
  , bxor
  , emptyArray
  )
where 
  
  import Control.Applicative
  import Control.Monad
  import Data.Array.Unboxed
  import Data.ByteString.Char8 (ByteString, pack)
  import Data.Monoid
  import Data.Word hiding (Word)
  import Prelude hiding ((++))
  
  -- words are 16 bits, pretty-printed in hexadecimal
  type Word = Word16
  
  -- RAM is a contiguous array of words.
  type RAM = UArray Word Word
  
  -- Registers are stored in contiguous memory
  type Slots = UArray Register Word
  
  data Register 
    = RA
    | RB
    | RC
    | RX
    | RY 
    | RZ 
    | RI 
    | RJ
    | PC
    | SP 
    | OF
    deriving (Eq, Ord, Enum, Bounded, Ix)
  
  instance Show Register where
    show RA = "A"
    show RB = "B"
    show RC = "C"
    show RX = "X"
    show RY = "Y"
    show RZ = "Z"
    show RI = "I"
    show RJ = "J"
    show PC = "PC"
    show SP = "SP"
    show OF = "O"
  
  data Opcode
    = JSR | SET | ADD | SUB | MUL | DIV | MOD 
    | AND | BOR | XOR | IFE | IFN | IFG | IFB
    deriving (Show, Eq, Ord, Enum, Bounded)
  
  -- Since all the leaves of this ADT are different, maybe this would be a 
  -- good place to try coproducts?
  data Operand 
    = AsmLiteral Int
    | AsmRegister Register
    | AsmReference Operand
    | AsmLabel ByteString
    deriving (Show, Eq)
  
  infixr 5 <>
  (<>) :: (Monoid a) => a -> a -> a
  (<>) = mappend
  
  bxor :: Bool -> Bool -> Bool
  bxor True True = False
  bxor False False = False
  bxor _ _ = True
  
  bshow :: (Show a) => a -> ByteString
  bshow = pack <$> show
  
  emptyArray :: (IArray a e, Ix t, Num e) => (t, t) -> a t e
  emptyArray minmax = array minmax [ (i, 0) | i <- range minmax ]
  
  data Instruction = Instruction { 
    label :: (Maybe ByteString),
    op :: Opcode,
    arga :: Operand,
    argb :: Operand
  } deriving (Show, Eq)
  