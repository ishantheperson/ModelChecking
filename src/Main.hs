{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
import ModelChecker.Parser
import ModelChecker.DFA 
import ModelChecker.AST
import ModelChecker.MakeTransducer

import SampleModel 
import Vector

import Data.List (intercalate)
import Data.Functor 
import Control.Monad 
import Control.Arrow ((>>>))

import System.IO

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
main = do 
  putStrLn "Please enter a sentence in FOL: "
  
  msentence <- getLine <&> parseString 

  case msentence of 
    Left err -> putStrLn $ show err 
    Right sentence -> let valid = mkTransducer (sentence)
                      in print valid -- mapM_ (putStrLn . intercalate "," . map (show . fromEnum)) valid 
                      -- in putStrLn $ "Formula is " ++ show valid 
#endif 
