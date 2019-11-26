{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module ModelChecker.Transducer where 

import Vector   
import ModelChecker.DFA 
import SampleModel 

-- | Extends the given DFA to one more track, using the given
--   mapping to control which "tracks" are actually used 
extend :: forall node sigma n. Vector n (Finite (Succ n)) -> DFA node sigma n -> DFA node sigma (Succ n)
extend mapping t = DFA (states t) arity' (isFinalState t) (isInitialState t) transitionFunction'
  -- Mapping would be something like (0, 2) which indicates
  -- that the first track on t gets its input from track 0 of the new extended DFA
  -- and that the second track on t gets its input from track 2 of the extended DFA 
  where arity' = SSucc (arity t)

        transitionFunction' :: (node, Vector (Succ n) sigma) -> [node]
        transitionFunction' (current, input) = 
          let delta = transitionFunction t 
          in delta (current, index input <$> mapping)

-- | Extends a DFA to several more tracks, using the given mapping to control which "tracks"
--   are actually used. It also takes in a witness to how many more tracks should be added.     
extendN :: forall node sigma n m. Vector n (Finite (n + m)) 
                               -> SNat (n + m) 
                               -> DFA node sigma n 
                               -> DFA node sigma (n + m)
extendN mapping m t = DFA (states t) arity' (isFinalState t) (isInitialState t) transitionFunction'
  -- This seems to work with AllowAmbiguousTypes, but if not then we need a witness for m
  -- which is the value of type (SNat m)
  where arity' = m -- arity t `addSnat` m --SSucc (arity t)

        transitionFunction' :: (node, Vector (n + m) sigma) -> [node]
        transitionFunction' (current, input) = 
          let delta = transitionFunction t 
          in delta (current, index input <$> mapping)

deleteTrack :: forall node sigma n. (Bounded sigma, Enum sigma) 
                                 => DFA node sigma n 
                                 -> Finite n 
                                 -> DFA node sigma n 
deleteTrack t i = t { transitionFunction = transitionFunction' }
  where transitionFunction' :: (node, Vector n sigma) -> [node]
        transitionFunction' (current, input) = 
          let replacedVectors = map (\c -> update input c i) [minBound..maxBound] 
          in concatMap (curry (transitionFunction t) current) replacedVectors 

eq3 = extend ($(mkFinite 0) :+ $(mkFinite 2) :+ VEmpty) eqDFA  
eq4 = extendN ($(mkFinite 0) :+ $(mkFinite 2) :+ VEmpty) $(mkSnat 4) eqDFA 