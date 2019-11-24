module SampleModel (addDFA) where 

import Util 
import ModelChecker.Transducer

import qualified Data.Set as Set 

data BinaryAlphabet = BZero | BOne deriving (Show, Eq, Ord, Bounded, Enum)
instance Num BinaryAlphabet where 
  fromInteger 0 = BZero
  fromInteger _ = BOne 
  -- TODO: add rest of operations 

data AddStates = Carry | NoCarry | Sink deriving (Show, Eq, Ord)

addStates = Set.fromList [Carry, NoCarry, Sink]
addIsFinal s = s == NoCarry 
addIsInitial s = s == NoCarry
addTransition (state, v) = case (state, fromVec3 v) of 
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
addDFA = Transducer addStates (SSucc (SSucc (SSucc SZero))) addIsFinal addIsInitial addTransition

data EqualStates = Equal | ESink deriving (Show, Eq, Ord)

-- Example:
-- >>> accepts eqDFA (map toVec2 [(0, 0), (1, 1), (0, 0)])
-- True
-- >>> accepts eqDFA (map toVec2 [(0, 0), (1, 1), (0, 1)])
-- False
eqDFA = Transducer eqStates (SSucc (SSucc SZero)) eqIsFinal eqIsInitial eqTransition
  where eqStates = Set.fromList [Equal, ESink]
        eqIsFinal = (==) Equal 
        eqIsInitial = (==) Equal 
        eqTransition (state, v) = case (state, fromVec2 v) of 
          (Equal, (0, 0)) -> Equal 
          (Equal, (1, 1)) -> Equal
          (Equal, _) -> ESink
          (ESink, _) -> ESink
        
