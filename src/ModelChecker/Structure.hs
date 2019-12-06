{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module ModelChecker.Structure where 

import ModelChecker.DFA   
import Vector 

data Structure (node :: *) (sigma :: *) = Structure {
  binOp :: Maybe (DFA node sigma (Succ (Succ Zero))),
  ternaryOp :: Maybe (DFA node sigma (Succ (Succ (Succ Zero)))),
  equalOp :: DFA node sigma (Succ (Succ Zero))
}

presburger = Structure { binOp = Just succDFA, ternaryOp = Just addDFA, equalOp = eqDFA }