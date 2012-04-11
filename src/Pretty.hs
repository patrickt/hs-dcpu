{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-} -- get rid of this

module Pretty where 
  
  import Core
  import Control.Applicative
  import Text.PrettyPrint
  import Text.Printf
  
  class Pretty a where
    pretty :: a -> Doc

  instance Pretty Word where
    pretty a = text $ printf "%04x" a

  instance Pretty RAM where
    pretty ramArray   = vcat $ renderChunk <$> [0, 8 .. limit ] 
      where
        limit           = (snd $ bounds ramArray) - 8
        octet pos       = map go [pos..pos+7] where go n = pretty $ (ramArray ! n)
        renderChunk pos = pretty pos <> colon <+> hsep (octet pos)
    
  instance Pretty Register where
    pretty = text . show


  instance Pretty Slots where
    pretty slots = vcat $ go <$> [RA .. OF] where
      go reg = pretty reg <> colon <+> pretty (slots ! reg)
