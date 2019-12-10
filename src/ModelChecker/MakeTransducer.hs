{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module ModelChecker.MakeTransducer where 

import ModelChecker.AST 
import ModelChecker.DFA 
import ModelChecker.Transducer 

import ModelChecker.Structure 

import SampleStructure 
import Vector 

import Data.List (intercalate)
import Data.Maybe (fromJust)

getVar :: Quantifier -> String 
getVar = \case 
  Forall s -> s 
  Exists s -> s 

toAdjacencyMatrix' t = let bools = toAdjacencyMatrix t 
  in "{" ++ intercalate "," (map (\line -> "{" ++ (intercalate ", " . map (show . fromEnum)) line ++ "}") bools) ++ "}"

valid :: (Ord t1, Ord t2, Ord t3) => Structure t1 t2 t3 BinaryAlphabet -> Statement -> Bool 
valid structure (Statement qs m) = withVector qs $ \len vars -> -- NOTE: qs is already reversed...or withIndices does it in reverse
  processMatrix structure m len (getVar <$> vars) $ \transducer -> 
    let withoutTracks = withIndices vars transducer $ 
          \index quant lastTransducer -> 
            case quant of Exists _ -> deleteTrack lastTransducer index
                          Forall _ -> negateMachine $ deleteTrack (negateMachine lastTransducer) index
    in nonempty withoutTracks 

-- This works but we need a full continuation  
processMatrix :: forall n b t1 t2 t3. (Ord t1, Ord t2, Ord t3) =>
                 Structure t1 t2 t3 BinaryAlphabet
              -> Matrix 
              -> SNat n 
              -> Vector n String 
              -> (forall ps. Ord ps => DFA ps BinaryAlphabet n -> b)
              -> b 
processMatrix structure m n names f = 
  case m of 
    TernaryOp a b c -> 
      f $ changeSize (indexOf names a :+ indexOf names b :+ indexOf names c :+ VEmpty) n (fromJust $ ternaryOp structure)
    RelatedTo a b -> 
      f $ changeSize (indexOf names a :+ indexOf names b :+ VEmpty) n (fromJust $ binOp structure)
    Equals a b -> 
      f $ changeSize (indexOf names a :+ indexOf names b :+ VEmpty) n (equalOp structure)
    Negation a -> 
      processMatrix structure a n names (f . negateMachine)
    And a b -> 
      processMatrix structure a n names $ \leftMachine -> 
        processMatrix structure b n names $ \rightMachine -> 
          f $ leftMachine `productMachine` rightMachine

-- Example:
-- >>> (Right m) = parse matrix "" "a -> b && a == c"
-- >>> f = processMatrix m $(mkSnat 4) ("a" :+ "c" :+ "b" :+ "d" :+ VEmpty)
-- >>> f (\mm -> accepts mm (map toVec4 [(0, 0, 0, 0)]))