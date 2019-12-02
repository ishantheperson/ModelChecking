{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module ModelChecker.MakeTransducer where 

import ModelChecker.AST 
import ModelChecker.DFA 
import ModelChecker.Transducer 
-- import ModelChecker.Parser

import SampleModel 
import Vector 

import Unsafe.Coerce  

--import Debug.Trace 

getVar :: Quantifier -> String 
getVar = \case 
  Forall s -> s 
  Exists s -> s 

-- FIXME: It seems that if there are extra variables, this gives the wrong answer. 
--        Might be due to how we always negate in a forall... 
mkTransducer :: Statement -> Bool 
mkTransducer (Statement qs m) = withVector (reverse qs) $ \len vars -> -- Reverse to eliminate quantifiers from inner to outer
  processMatrix m len (getVar <$> vars) $ \transducer ->  --(not . empty)
    let withoutTracks = withIndices vars transducer $ 
          \index quant lastTransducer -> 
            case quant of Exists _ -> deleteTrack lastTransducer index
                          -- Need to logically negate the _result_ not the machine?
                          -- forall x. phi is the same as exists not x. !phi 
                          -- But one of those is negating the matrix...but
                          -- the other one is just negating the result directly?
                          -- (the truth value of the expression)
                          -- This current solution seems hackish. Maybe instead we could 
                          -- in order to get rid of this? That seems like a better solution. 
                          
                          -- The issue is in something like
                          -- > forall a. a + a = a === !(exists a. a + a = a)
                          -- It's true that 
                          -- > exists a. a + a = a
                          -- and also that 
                          -- > exists a. !(a + a = a)
                          -- Do we have to "evaluate" the inner negation first? 
                          -- This doesn't always seem possible.
                          -- > forall b. forall a. (b = b) => (a + a = a)
                          -- => forall b. ! (exists a. !(b = b => (a + a = a))) 
                          Forall _ -> negateMachine $ deleteTrack (negateMachine lastTransducer) index
    in (not . empty) withoutTracks -- show $ getInitialState withoutTracks --toAdjacencyMatrix withoutTracks

-- This works but we need a full continuation  
processMatrix :: forall n b. Matrix 
                          -> SNat n 
                          -> Vector n String 
                          -> (forall ps. (Show ps, Ord ps) => DFA ps BinaryAlphabet n -> b)
                          -> b 
processMatrix m n names f = 
  case m of 
    TernaryOp a b c -> 
      f (changeSize (indexOf names a id :+ indexOf names b id :+ indexOf names c id :+ VEmpty) n addDFA)
    Equals (Variable a) (Variable b) -> 
      f (changeSize (indexOf names a id :+ indexOf names b id :+ VEmpty) n eqDFA)
    Negation a -> 
      processMatrix a n names (f . negateMachine)
    And a b -> 
      processMatrix a n names $ \leftMachine -> 
        processMatrix b n names $ \rightMachine -> 
          f $ leftMachine `productMachine` rightMachine

-- This works but we need a full continuation  
-- processMatrix :: forall n b. Matrix 
--                           -> SNat (Succ (Succ n)) 
--                           -> Vector (Succ (Succ n)) String 
--                           -> (forall ps. Ord ps => DFA ps BinaryAlphabet (Succ (Succ n)) -> b) 
--                           -> b 
-- processMatrix m n names f = 
--   case m of 
--     RelatedTo (Variable a) (Variable b) -> 
--       f (extendN (indexOf names a id :+ indexOf names b id :+ VEmpty) n equalParityDFA)
--     Equals (Variable a) (Variable b) -> 
--       f (extendN (indexOf names a id :+ indexOf names b id :+ VEmpty) n eqDFA)
--     Negation a -> 
--       processMatrix a n names (f . negateMachine)
--     And a b -> 
--       processMatrix a n names $ \leftMachine -> 
--         processMatrix b n names $ \rightMachine -> 
--           f $ leftMachine `productMachine` rightMachine

-- Example:
-- >>> (Right m) = parse matrix "" "a -> b && a == c"
-- >>> f = processMatrix m $(mkSnat 4) ("a" :+ "c" :+ "b" :+ "d" :+ VEmpty)
-- >>> f (\mm -> accepts mm (map toVec4 [(0, 0, 0, 0)]))