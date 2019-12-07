{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

#if __GLASGOW_HASKELL__ < 800 
#warning "GHC v8.0 is required to build this project"
#endif 

import ModelChecker.Parser
import ModelChecker.MakeTransducer
import ModelChecker.Structure 

import SampleStructure 

import System.Console.Haskeline

main :: IO () 
main = runInputT settings $ do 
  outputStrLn "Please enter a sentence in FOL, or type \"help\" for help: "
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
          msentence <- getInputLine "> " 
          case msentence of 
            Nothing -> return () 
            Just "" -> loop 
            Just "help" -> printHelp 
            Just (parseString -> Left err) -> do 
              outputStrLn $ show err 
              outputStrLn "" 
              loop 

            Just (parseString -> Right program) -> do 
              case checkStatement presburger program of 
                [] -> return () 
                errors -> do 
                  mapM_ outputStrLn errors 
                  outputStrLn ""
                  loop 

              let result = valid presburger program 
              outputStrLn $ show result 
              outputStrLn ""
              loop 
