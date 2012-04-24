module Main where
  
  import Core
  import Assembler
  import Parser
  import Pretty
  import Data.Maybe
  import System.Console.Readline
  import Text.Parsec (parseTest)
  
  main :: IO ()
  main = do
    putStrLn "hs-dcpu started in interactive mode. Ctrl-D exits."
    inputLoop
    where
      inputLoop :: IO ()
      inputLoop = do
        p <- readline ">>> "
        case p of
          Nothing -> return ()
          Just prog -> do
            let e = parse program "" (pack prog)
            case e of
              (Left err) -> print err
              (Right pro) -> do
                print pro
                let asm = doAssembly pro
                print $ pretty <$> asm
            inputLoop
  