module Core 
  ( Word
  , RAM 
  , Register (..)
  , Slots
  , module Data.Array.Unboxed
  , module Data.Word
  )
where 
  
  import Data.Array.Unboxed
  import Data.Word hiding (Word)
  
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
    deriving (Show, Eq, Ord, Enum, Ix)