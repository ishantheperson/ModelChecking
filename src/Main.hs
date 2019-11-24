{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
import ModelChecker.Parser
import ModelChecker.Transducer 

import SampleModel 
import Util

import Control.Monad 
import System.IO

#define TEST_PARSER 0 
#define TEST_DFA 1 

#define CURRENT_TEST TEST_DFA 

main :: IO () 
#if CURRENT_TEST == TEST_PARSER
main = print =<< parseString <$> getContents 
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
          unless condition $ putStrLn $ "error: " ++ msg 
#endif 
