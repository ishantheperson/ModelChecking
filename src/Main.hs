{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

#if __GLASGOW_HASKELL__ < 800 
#warning "GHC v8.0 is required to build this project"
#endif 

import ModelChecker.Parser
import ModelChecker.MakeTransducer
import ModelChecker.Matrix 
import ModelChecker.Structure 

import SampleStructure 

import System.Console.Haskeline

defaultStructure = presburger

main :: IO () 
main = runInputT settings $ do 
  outputStrLn "Please enter a sentence (or just the matrix) in FOL, or type \"help\" for help: "
  loop

  where settings = defaultSettings { historyFile = Just ".mcheck_history" } 
        printHelp = do 
          outputStrLn "Example formulas:"
          outputStrLn "forall a b c. a -> b && a -> c => b = c"
          outputStrLn "exists a. forall b. a + b = a"
          outputStrLn "forall a b c. a + b = c => a != c"
          outputStrLn "forall a b c. a + b = c => b + a = c"
          outputStrLn ""
          loop
  
        loop = do 
          msentence <- getInputLine "\x1b[34;1m> \x1b[0m" 
          case msentence of 
            Nothing -> outputStrLn "Goodbye"
            Just "" -> loop 
            Just "help" -> printHelp 
            Just (parseString -> Left err) -> do 
              outputStrLn $ show err 
              outputStrLn "" 
              loop 

            Just (parseString -> Right parseResult) -> do 
              case parseResult of 
                ParsedMatrix m -> 
                  case testMatrix defaultStructure m of 
                    Left errors -> mapM_ outputStrLn errors 
                    Right result -> outputStrLn result 

                ParsedStatement program -> 
                  case checkStatement defaultStructure program of 
                    [] -> do            
                      let result = valid defaultStructure program 
                      outputStrLn $ show result 

                    errors -> mapM_ outputStrLn errors 

              outputStrLn ""
              loop 
