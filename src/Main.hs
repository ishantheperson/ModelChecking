{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

#if __GLASGOW_HASKELL__ < 800 
#warning "GHC v8.0 is required to build this project"
#endif 

import ModelChecker.Parser
import ModelChecker.DFA 
import ModelChecker.AST
import ModelChecker.MakeTransducer

import SampleModel 
import Vector

import Data.List (intercalate)
import Data.Functor 
import Control.Monad 
import Control.Monad.IO.Class
import Control.Arrow ((>>>))

import System.IO
import System.Console.Haskeline

-- Prints out parse tree
#define TEST_PARSER 0     
-- Tests addDFA
#define TEST_DFA 1        
-- Test model checker 
#define TEST_TRANSDUCER 2 

#define CURRENT_TEST TEST_TRANSDUCER 

main :: IO () 
#if CURRENT_TEST == TEST_PARSER
main = putStrLn =<< printStatement . parseString <$> getContents 
#elif CURRENT_TEST == TEST_DFA 
main = do
  putStrLn "Testing a + b = c"
  putStrLn "Enter space-seperated binary digits (not reversed)"

  a <- readBits "a"
  b <- readBits "b"
  c <- readBits "c"

  ioGuard (length a == length b && length b == length c)
    "digit lists must all have the same length"

  if accepts addDFA (map toVec3 $ zip3 a b c)
    then putStrLn "Accepted!"
    else putStrLn "Rejected :("

  where readBits :: String -> IO [BinaryAlphabet]
        readBits s = do 
          putStr $ s ++ ": "
          hFlush stdout
          input <- map (fromInteger . read) . words <$> getLine 

          return $ reverse input 

        ioGuard :: Bool -> String -> IO () 
        ioGuard condition msg = 
          unless condition $ do putStrLn $ "error: " ++ msg 
                                exitFailure 

#elif CURRENT_TEST == TEST_TRANSDUCER 
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
              loop 

            Just (parseString -> Right program) -> do 
              let valid = mkTransducer program 
              outputStrLn $ show valid 
              loop 
#endif 
