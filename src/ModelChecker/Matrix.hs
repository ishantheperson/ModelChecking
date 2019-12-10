{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module ModelChecker.Matrix where 

import ModelChecker.AST 
import ModelChecker.DFA 
import ModelChecker.MakeTransducer 
import ModelChecker.Structure 

import Vector 

import Data.Set (Set)
import qualified Data.Set as Set 

import Control.Monad.State 
import Control.Monad.Writer

-- | Gets all variables seen in the matrix 
getAllVars structure matrix = 
  case runWriter $ execStateT (go matrix) Set.empty of 
    (vars, []) -> Right $ Set.toList vars 
    (_, errors) -> Left $ errors 

  where go :: (MonadState (Set String) m, MonadWriter [String] m) => Matrix -> m () 
        go = \case 
          Negation m -> go m 
          And m1 m2 -> go m1 >> go m2 
          RelatedTo a b -> case binOp structure of 
            Nothing -> tell ["binary operation not supported"]
            Just _  -> forM_ [a, b] (modify . Set.insert) 

          TernaryOp a b c -> case ternaryOp structure of 
            Nothing -> tell ["ternary operation not supported"]
            Just _  -> forM_ [a, b, c] (modify . Set.insert) 

          Equals a b -> forM_ [a, b] (modify . Set.insert)  

testMatrix structure matrix = 
  case getAllVars structure matrix of 
    Left errs -> Left errs 
    Right vars -> withVector vars $ \n v ->
      processMatrix structure matrix n v $ \transducer -> Right $ 
        case findAccepted transducer of 
          Nothing -> "<language is empty>"
          Just [] -> "<empty string>"
          Just s -> 
            withIndices v "" $ \i name accum -> accum ++ (if null accum then "" else "\n") ++ 
              name ++ " -> " ++ unwords (map (show . flip index i) s)