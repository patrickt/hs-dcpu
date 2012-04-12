module Core 
  ( Word
  , RAM 
  , Register (..)
  , Slots
  , module Control.Applicative
  , module Control.Monad
  , module Data.Array.Unboxed
  , module Data.Monoid
  , module Data.Word
  , (++)
  , bshow
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
  
  infixr 5 ++
  (++) :: (Monoid a) => [a] -> a
  (++) = mconcat
  
  bshow :: (Show a) => a -> ByteString
  bshow = pack <$> show
  