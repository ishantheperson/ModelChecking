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

import SampleModel 
import Vector 

getVar :: Quantifier -> String 
getVar = \case 
  Forall s -> s 
  Exists s -> s 

mkTransducer :: Statement -> Bool 
mkTransducer (Statement qs m) = withVector qs $ \len vars -> -- NOTE: qs is already reversed...or withIndices does it in reverse
  processMatrix m len (getVar <$> vars) $ \transducer -> 
    let withoutTracks = withIndices vars transducer $ 
          \index quant lastTransducer -> -- traceShow (activeTracks lastTransducer) $
            case quant of Exists _ -> deleteTrack lastTransducer index
                          Forall _ -> negateMachine $ deleteTrack (negateMachine lastTransducer) index
    in nonempty withoutTracks 

-- This works but we need a full continuation  
processMatrix :: forall n b. Matrix 
                          -> SNat n 
                          -> Vector n String 
                          -> (forall ps. (Show ps, Ord ps) => DFA ps BinaryAlphabet n -> b)
                          -> b 
processMatrix m n names f = 
  case m of 
    TernaryOp a b c -> 
      f $ changeSize (indexOf names a :+ indexOf names b :+ indexOf names c :+ VEmpty) n addDFA
    RelatedTo a b -> 
      f $ changeSize (indexOf names a :+ indexOf names b :+ VEmpty) n succDFA
    Equals a b -> 
      f $ changeSize (indexOf names a :+ indexOf names b :+ VEmpty) n eqDFA
    Negation a -> 
      processMatrix a n names (f . negateMachine)
    And a b -> 
      processMatrix a n names $ \leftMachine -> 
        processMatrix b n names $ \rightMachine -> 
          f $ leftMachine `productMachine` rightMachine

-- Example:
-- >>> (Right m) = parse matrix "" "a -> b && a == c"
-- >>> f = processMatrix m $(mkSnat 4) ("a" :+ "c" :+ "b" :+ "d" :+ VEmpty)
-- >>> f (\mm -> accepts mm (map toVec4 [(0, 0, 0, 0)]))