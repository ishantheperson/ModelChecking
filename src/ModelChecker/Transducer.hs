{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
module ModelChecker.Transducer where 

import Vector   
import ModelChecker.DFA 
import SampleModel 

import Data.List (nub)

-- | Changes the size of a DFA, given a mapping to the new tracks.
--   e.g. changeSize (FZer0 :+ FZero :+ VEmpty) $(mkSnat 1) eqDFA
--   would make eqDFA operate on tracks 0 and 0 (i.e. it will always accept)
changeSize :: forall node sigma n m. Vector n (Finite m)
                                  -> SNat m 
                                  -> DFA node sigma n 
                                  -> DFA node sigma m 
changeSize mapping m t = t { arity = m, transitionFunction = transitionFunction'}                                            
  where transitionFunction' :: (node, Vector m sigma) -> [node]
        transitionFunction' (current, input) = 
          let delta = transitionFunction t 
          in delta (current, index input <$> mapping)

-- Quantify "exists" on transducer @t@ on track @i@           
deleteTrack :: forall node sigma n. (Bounded sigma, Enum sigma, Eq node) 
                                 => DFA node sigma n 
                                 -> Finite n 
                                 -> DFA node sigma n 
deleteTrack transducer i = transducer { transitionFunction = transitionFunction' }
  where transitionFunction' :: (node, Vector n sigma) -> [node]
        transitionFunction' (current, input) = 
          let replacedVectors = map (\c -> update input c i) [minBound..maxBound] 
          in nub $ concatMap (curry (transitionFunction transducer) current) replacedVectors 

        -- FIXME: The bug is a problem in the accepting condition of nondeterminism. 
        -- isFinalState' = undefined 

-- Example:           
-- eq4 = changeSize ($(mkFinite 0) :+ $(mkFinite 2) :+ VEmpty) $(mkSnat 4) eqDFA 