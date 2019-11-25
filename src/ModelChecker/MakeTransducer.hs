{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module ModelChecker.MakeTransducer where 

import ModelChecker.AST 
import ModelChecker.DFA 
import ModelChecker.Transducer 
import ModelChecker.Parser


import SampleModel 

import Vector 

getVar :: Quantifier -> String 
getVar = \case 
  Forall s -> s 
  Exists s -> s 

{-mkTransducer :: forall n ps. Statement -> DFA ps BinaryAlphabet (Succ (Succ n))
mkTransducer (Statement qs m) = withVector (map getVar qs) $ \len vars -> 
  case len of 
    SZero -> error "need more vars"
-   SSucc SZero -> error "need more vars"
    SSucc (SSucc i) -> processMatrix m (SSucc (SSucc i)) vars (id)
-}

processMatrix :: forall n b. Matrix -> SNat (Succ (Succ n)) -> Vector (Succ (Succ n)) String -> (forall ps. DFA ps BinaryAlphabet (Succ (Succ n)) -> b) -> b 
processMatrix m n names f = 
  case m of 
    RelatedTo (Variable a) (Variable b) -> 
      f (extendN (indexOf names a id :+ indexOf names b id :+ VEmpty) n equalParityDFA)
    Equals (Variable a) (Variable b) -> 
      f (extendN (indexOf names a id :+ indexOf names b id :+ VEmpty) n eqDFA)
    Negation a -> 
      processMatrix a n names $ \trds -> 
        f $ negateMachine trds -- FIXME: variable name
    And a b -> 
      processMatrix a n names $ \leftMachine -> 
        processMatrix b n names $ \rightMachine -> 
          f $ leftMachine `productMachine` rightMachine

