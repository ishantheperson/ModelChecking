module SampleModel (addDFA) where 

import Util 
import ModelChecker.Transducer

import Data.Set (Set)
import qualified Data.Set as Set 

data BinaryAlphabet = Zero | One deriving (Show, Eq, Ord, Bounded, Enum)

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

eqStates = Set.fromList [Equal, ESink]
eqAlphabet = Set.fromList $ [(0, 0), (0, 1), (1, 0), (1, 1)]


