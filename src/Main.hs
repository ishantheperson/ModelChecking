{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

#if __GLASGOW_HASKELL__ < 800 
#error "GHC v8.0 is required to build"
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
  outputStrLn "Please enter a sentence in FOL: "
  loop

  where settings = defaultSettings { historyFile = Just ".mcheck_history" }                      
        loop = do 
          msentence <- getInputLine "> " <&> (fmap parseString)

          case msentence of 
            Nothing -> return () 
            Just (Left err) -> do 
              outputStrLn $ show err
              loop 
            Just (Right sentence) -> do 
              let valid = mkTransducer sentence
              outputStrLn $ show valid -- mapM_ (putStrLn . intercalate "," . map (show . fromEnum)) valid 
              loop   -- in putStrLn $ "Formula is " ++ show valid 

#endif 
