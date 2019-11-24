{-# LANGUAGE LambdaCase #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module ModelChecker.DFA (
  DFA(..), 
  toAdjacencyMatrix,
  accepts, empty, 
  productMachine, negateMachine) where 

import Util 

import Data.Set (Set, (\\))
import qualified Data.Set as Set 
import Data.Foldable (find)

import Control.Monad.State 

-- | Represents a nondeterministic automata
-- 
-- @node@: Type of nodes
-- @sigma@: Type of transition labels (alphabet)
data DFA (node :: *) (sigma :: *) (arity :: Nat) = DFA {
  states :: Set node,
  arity :: SNat arity,
  isFinalState :: node -> Bool,
  isInitialState :: node -> Bool,
  transitionFunction :: (node, Vector arity sigma) -> [node]
}

-- | Renders an approximation of the DFA's graph - there are no edge labels
--   since the website I found didn't support that. 
--   TODO: Look into networkx (python package) and use that to render a cool
--         graph 
--      @ mapM_ (putStrLn . intercalate ", ") $ map (map (show . fromEnum)) $ toAdjacencyMatrix moreThanThreeDFA
toAdjacencyMatrix :: (Eq node, Ord node, Ord sigma, Bounded sigma, Enum sigma) => 
                      DFA node sigma arity -> [[Bool]]
toAdjacencyMatrix t = map processState nodeList
  where nodeList = Set.toList $ states t
        processState s = 
          let reachable = getDestinations t s 
          in map (flip Set.member reachable) nodeList 

-- | Gets the initial state of the DFA by using its @isInitialState@ function.
--   Precondition: @isInitialState$ must return @True@ for at least one state in
--   @states@
getInitialState t = 
  case find (isInitialState t) (states t) of 
    Just s -> s 
    Nothing -> error "No initial state found in given automata!"

-- | Gets the set of all states reachable from this one 
getDestinations :: (Ord node, Ord sigma, Bounded sigma, Enum sigma) => 
                      DFA node sigma arity -> node -> Set node 
getDestinations t n = 
  Set.fromList $ concatMap (curry (transitionFunction t) n) (getAlphabet (arity t))

-- | Tests whether the DFA accepts a given string   
accepts :: forall node sigma arity. DFA node sigma arity -> [Vector arity sigma] -> Bool 
accepts t = go [getInitialState t]
  where go :: [node] -> [Vector arity sigma] -> Bool
        go subset = \case 
          [] -> any (isFinalState t) subset  
          x:xs -> go (concatMap (\s -> transitionFunction t (s, x)) subset) xs 

-- | Tests whether the language of the DFA is empty
--   by performing DFS           
empty :: forall node sigma arity. (Ord node, Ord sigma, Bounded sigma, Enum sigma) => 
              DFA node sigma arity -> Bool
empty t = Set.null . Set.filter (isFinalState t) $ reachable
  where dfs :: MonadState (Set node) m => node -> [node] -> m ()
        dfs currentNode next = do 
          visited <- get 

          if Set.member currentNode visited 
            then return ()
            else do 
              modify $ Set.insert currentNode 

              let destinations = getDestinations t currentNode \\ visited 
              case Set.toList destinations <> next of 
                [] -> return () 
                x:xs -> dfs x xs 

        reachable :: Set node
        reachable = execState (dfs (getInitialState t) []) Set.empty   

-- | Constructions a DFA from t1 and t2 
--   where L(productMachine t1 t2) = L(t1) intersect L(t2)       
productMachine :: DFA n1 b c -> DFA n2 b c -> DFA (n1, n2) b c
productMachine t1 t2 = DFA states' arity' isFinalState' isInitialState' transitionFunction' 
  where states' = states t1 `Set.cartesianProduct` states t2
        arity' = arity t1
        isFinalState' (n1, n2) = isFinalState t1 n1 && isFinalState t2 n2 
        isInitialState' (n1, n2) = isInitialState t1 n1 && isInitialState t2 n2 

        transitionFunction' ((n1, n2), e) = [(a, b) | a <- transitionFunction t1 (n1, e),
                                                      b <- transitionFunction t2 (n2, e) ]

-- | Constructions the complement of a DFA           
negateMachine :: DFA a b c -> DFA a b c
negateMachine t1 = t1 { isFinalState = not . isFinalState t1 }