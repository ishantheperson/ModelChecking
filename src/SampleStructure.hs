{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module SampleStructure where 

import ModelChecker.DFA
import ModelChecker.Structure 
  
import Vector 
import qualified Data.Set as Set 

-- | Theory of Presburger arithmetic, or just arithmetic with only addition. 
-- 
-- \( a \to b \implies a + 1 = b \)
presburger = Structure { binOp = Just succDFA, ternaryOp = Just addDFA, equalOp = eqDFA }

data BinaryAlphabet = BZero | BOne deriving (Eq, Ord, Bounded, Enum)
instance Show BinaryAlphabet where 
  show BZero = "0"
  show BOne  = "1"

instance Num BinaryAlphabet where 
  fromInteger 0 = BZero
  fromInteger _ = BOne 

  BZero + BZero = BZero
  BOne + BOne = BZero
  _ + _ = BOne

  BOne * BOne = BOne
  _ * _ = BZero

  abs = id 
  signum = id 
  negate = id 

data AddState = Carry | NoCarry | Sink deriving (Show, Eq, Ord)

addStates = Set.fromList [Carry, NoCarry, Sink]
addIsFinal s = s == NoCarry 
addIsInitial s = s == NoCarry
addTransition (state, v) = pure $ case (state, fromVec3 v) of 
  (Sink, _) -> Sink 
  (Carry, (0, 0, 1)) -> NoCarry 
  (Carry, (1, 1, 0)) -> Carry
  (Carry, (0, 1, 0)) -> Carry
  (Carry, (1, 0, 0)) -> Carry
  (NoCarry, (1, 1, 0)) -> Carry
  (NoCarry, (0, 0, 0)) -> NoCarry
  (NoCarry, (1, 0, 1)) -> NoCarry
  (NoCarry, (0, 1, 1)) -> NoCarry
  _ -> Sink 

addDFA :: DFA AddState BinaryAlphabet (Succ (Succ (Succ Zero)))
addDFA = DFA addStates $(mkSnat 3) addIsFinal addIsInitial addTransition (new $(mkSnat 3) True)

succDFA :: DFA AddState BinaryAlphabet (Succ (Succ Zero))
succDFA = DFA addStates $(mkSnat 2) (== NoCarry) (== Carry) succTransition (new $(mkSnat 2) True)
  where succTransition (state, v) = pure $ case (state, fromVec2 v) of 
          (Sink, _) -> Sink 
          (Carry, (0, 1)) -> NoCarry
          (Carry, (1, 0)) -> Carry
          (NoCarry, (0, 0)) -> NoCarry
          (NoCarry, (1, 1)) -> NoCarry
          _ -> Sink 

data EqualStates = Equal | ESink deriving (Show, Eq, Ord)

eqDFA :: DFA EqualStates BinaryAlphabet (Succ (Succ Zero))
eqDFA = DFA eqStates $(mkSnat 2) eqIsFinal eqIsInitial eqTransition (new $(mkSnat 2) True)
  where eqStates = Set.fromList [Equal, ESink]
        eqIsFinal = (==) Equal 
        eqIsInitial = (==) Equal 
        eqTransition (state, v) = (:[]) $ case (state, fromVec2 v) of 
          (Equal, (0, 0)) -> Equal 
          (Equal, (1, 1)) -> Equal
          (Equal, _) -> ESink
          (ESink, _) -> ESink
        