module Main where
  
  import Core
  import Assembler
  import Parser
  import Pretty
  import Text.Parsec (parseTest)
  
  main :: IO ()
  main = do
    putStrLn "hs-dcpu started in interactive mode. Ctrl-C exits."
    inputLoop
    where
      inputLoop :: IO ()
      inputLoop = do
        putStr ">>> "
        prog <- getLine
        let e = parse program "" (pack prog)
        case e of
          (Left err) -> print err
          (Right pro) -> do
            print pro
            let asm = doAssembly pro
            print $ pretty <$> asm
            
        inputLoop
  