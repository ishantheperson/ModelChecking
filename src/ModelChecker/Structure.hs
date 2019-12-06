{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module ModelChecker.Structure where 

import ModelChecker.DFA   
import SampleModel 
import Vector 

data Structure a b c sigma = Structure {
  binOp :: Maybe (DFA a sigma (Succ (Succ Zero))),
  ternaryOp :: Maybe (DFA b sigma (Succ (Succ (Succ Zero)))),
  equalOp :: DFA c sigma (Succ (Succ Zero))
}

--presburger :: Structure 
presburger = Structure { binOp = Just succDFA, ternaryOp = Just addDFA, equalOp = eqDFA }