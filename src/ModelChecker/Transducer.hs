{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module ModelChecker.Transducer where 

import Util   
import ModelChecker.DFA 
import SampleModel 

-- | Extends the given DFA to one more track, using the given
--   mapping to control which "tracks" are actually used 
extend :: Vector n (Finite (Succ n)) -> DFA node sigma n -> DFA node sigma (Succ n)
extend mapping t = DFA (states t) arity' (isFinalState t) (isInitialState t) transitionFunction'
  -- mapping tells us which 
  where arity' = SSucc (arity t)
        -- transitionFunction' :: (node, Vector (Succ n) sigma) -> [node]
        --transitionFunction' :: (node, Vector (Succ n) sigma) -> [node]
        transitionFunction' (c, input) = 
          let delta = transitionFunction t 
          in delta (c, fmap (index input) mapping)

eq3 = extend ($(mkFinite 0) :+ $(mkFinite 2) :+ VEmpty) eqDFA          