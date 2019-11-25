{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module SampleModel where 

import Vector 
import ModelChecker.DFA

import qualified Data.Set as Set 

data BinaryAlphabet = BZero | BOne deriving (Show, Eq, Ord, Bounded, Enum)
instance Num BinaryAlphabet where 
  fromInteger 0 = BZero
  fromInteger _ = BOne 
  -- TODO: add rest of operations 

data AddState = Carry | NoCarry | Sink deriving (Show, Eq, Ord)

addStates = Set.fromList [Carry, NoCarry, Sink]
addIsFinal s = s == NoCarry 
addIsInitial s = s == NoCarry
addTransition (state, v) = pure $ case (state, fromVec3 v) of 
  (Sink, _) -> Sink 
  (Carry, (1, 0, 1)) -> NoCarry
  (Carry, (0, 1, 1)) -> NoCarry
  (Carry, (0, 0, 1)) -> NoCarry 
  (Carry, (1, 1, 0)) -> Carry
  (NoCarry, (1, 1, 0)) -> Carry
  (NoCarry, (0, 0, 0)) -> NoCarry
  (NoCarry, (1, 0, 1)) -> NoCarry
  (NoCarry, (0, 1, 1)) -> NoCarry
  _ -> Sink 

-- Example: 
-- >>> accepts addDFA (map toVec3 [(1, 1, 0), (0, 0, 1)])
-- True
-- addDFA = DFA addStates (SSucc (SSucc (SSucc SZero))) addIsFinal addIsInitial addTransition
addDFA :: DFA AddState BinaryAlphabet (Succ (Succ (Succ Zero)))
addDFA = DFA addStates $(mkSnat 3) addIsFinal addIsInitial addTransition

data EqualStates = Equal | ESink deriving (Show, Eq, Ord)

-- Example:
-- >>> accepts eqDFA (map toVec2 [(0, 0), (1, 1), (0, 0)])
-- True
-- >>> accepts eqDFA (map toVec2 [(0, 0), (1, 1), (0, 1)])
-- False
eqDFA = DFA eqStates $(mkSnat 2) eqIsFinal eqIsInitial eqTransition
  where eqStates = Set.fromList [Equal, ESink]
        eqIsFinal = (==) Equal 
        eqIsInitial = (==) Equal 
        eqTransition (state, v) = (:[]) $ case (state, fromVec2 v) of 
          (Equal, (0, 0)) -> Equal 
          (Equal, (1, 1)) -> Equal
          (Equal, _) -> ESink
          (ESink, _) -> ESink
        
moreThanThreeDFA :: DFA Int BinaryAlphabet (Succ Zero)          
moreThanThreeDFA = DFA states $(mkSnat 1) (==3) (==1) delta 
  where states = Set.fromList [1..3]
        delta (state, _) = (:[]) $ case state of 
          1 -> 2
          2 -> 3
          3 -> 3