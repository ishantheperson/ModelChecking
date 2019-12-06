{-# LANGUAGE LambdaCase #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module ModelChecker.DFA (
  DFA(..), 
  -- toAdjacencyMatrix,
  empty, nonempty,
  getInitialState,
  nfaeClosure,
  productMachine, negateMachine) where 

import Vector 

import Data.List (nub)
import Data.Set (Set)
import qualified Data.Set as Set 

import Control.Monad.State 

-- | Represents a nondeterministic automata
-- 
-- @node@: Type of nodes
-- @sigma@: Type of transition labels (alphabet)
-- @arity@: Number of tracks
data DFA (node :: *) (sigma :: *) (arity :: Nat) = DFA {
  states :: Set node,
  arity :: SNat arity,
  isFinalState :: node -> Bool,
  isInitialState :: node -> Bool,
  transitionFunction :: (node, Vector arity sigma) -> [node],
  activeTracks :: Vector arity Bool 
}

-- | Gets a version of the DFAs transition function which takes into account
--   deleted tracks. If all tracks are deleted, then every transition is an 
--   epsilon transition 
mkTransition :: forall node sigma arity. (Bounded sigma, Enum sigma, Eq node) 
                                      => DFA node sigma arity 
                                      -> (node, Vector arity sigma) 
                                      -> [node]
mkTransition t (node, letter) = 
      let vectors = mkVectors (activeTracks t) letter 
      in nub $ concatMap (\v -> delta (node, v)) vectors     
  where delta = transitionFunction t 

        mkVectors :: forall n sigma. (Bounded sigma, Enum sigma) => Vector n Bool -> Vector n sigma -> [Vector n sigma]
        mkVectors VEmpty         VEmpty    = [VEmpty]
        mkVectors (True  :+ bs)  (x :+ xs) = (:+) <$> [x] <*> mkVectors bs xs
        mkVectors (False :+ bs)  (_ :+ xs) = (:+) <$> ([minBound..maxBound] :: [sigma]) <*> mkVectors bs xs

-- | Renders an approximation of the DFA's graph - there are no edge labels
--   since the website I found didn't support that. 
--   TODO: Look into networkx (python package) and use that to render a cool
--         graph 
--      @ mapM_ (putStrLn . intercalate ", ") $ map (map (show . fromEnum)) $ toAdjacencyMatrix moreThanThreeDFA
-- toAdjacencyMatrix :: (Eq node, Ord node, Ord sigma, Bounded sigma, Enum sigma, Show node) => 
--                       DFA node sigma arity -> [[Bool]]
-- toAdjacencyMatrix t = map processState nodeList
--   where nodeList = Set.toList $ states t
--         processState s = 
--           let reachable = getDestinations t s 
--           in map (`Set.member` reachable) nodeList 

-- | Gets the initial state of the DFA by using its @isInitialState@ function.
--   Precondition: @isInitialState$ must return @True@ for at least one state in
--   @states@
getInitialState t = 
  case Set.toList $ Set.filter (isInitialState t) (states t) of 
    [] -> error "No initial state found in given automata!"
    other -> other 

-- | Gets the set of all states reachable from this one 
getDestinations :: (Ord node, Ord sigma, Bounded sigma, Enum sigma) 
                => DFA node sigma arity -> node -> Set node 
getDestinations t n = 
  Set.fromList $ concatMap (curry (mkTransition t) n) (getAlphabet (arity t))

-- | Tests whether the DFA accepts a given string   
-- accepts :: forall node sigma arity. (Bounded sigma, Enum sigma, Eq node) => DFA node sigma arity -> [Vector arity sigma] -> Bool 
-- accepts t = go $ getInitialState t -- TODO: possibly multiple initial states 
--   where go :: [node] -> [Vector arity sigma] -> Bool -- FIXME: This should be a set of states
--         go subset = \case 
--           [] -> any (isFinalState t) subset  
--           x:xs -> go (nub $ concatMap (\s -> mkTransition t (s, x)) subset) xs 

-- | Tests whether the language of the DFA is empty
--   by performing DFS           
empty, nonempty :: forall node sigma arity. 
                   (Ord node, Ord sigma, Bounded sigma, Enum sigma) 
                => DFA node sigma arity -> Bool
empty t = Set.null $ Set.filter (isFinalState t) reachable
  where dfs :: MonadState (Set node) m => node -> m () 
        dfs currentNode = do 
          modify $ Set.insert currentNode

          forM_ (Set.toList $ getDestinations t currentNode) $ \v -> do 
            visited <- get 

            if Set.member v visited 
              then return () 
              else dfs v  

        reachable :: Set node
        reachable = 
          let initial = getInitialState t 
          in execState (traverse dfs initial) Set.empty

nonempty = not . empty 

-- | Constructs a DFA from t1 and t2 
--   where \( L(t1 \texttt{ `productMachine` } t2) = L(t_1) \cap L(t_2) \) 
-- 
-- Precondition: We assume activeTracks t1 == activeTracks t2 
productMachine :: (Eq n1, Eq n2) => DFA n1 b c -> DFA n2 b c -> DFA (n1, n2) b c
productMachine t1 t2 = DFA states' arity' isFinalState' isInitialState' transitionFunction' activeTracks'
  where states' = states t1 `Set.cartesianProduct` states t2
        arity'  = arity t1
        activeTracks' = activeTracks t1 
        isFinalState'   (n1, n2) = isFinalState   t1 n1 && isFinalState   t2 n2 
        isInitialState' (n1, n2) = isInitialState t1 n1 && isInitialState t2 n2 

        transitionFunction' ((n1, n2), e) = nub [(a, b) | a <- transitionFunction t1 (n1, e),
                                                          b <- transitionFunction t2 (n2, e) ]

nfaeClosure :: (Ord a, Ord b, Bounded b, Enum b) => DFA a b c -> DFA a b c 
nfaeClosure t =
  if or $ activeTracks t
    then t 
    else
      DFA (states t) (arity t) (const emptiness) (const True) (transitionFunction t) (activeTracks t)

  where emptiness = nonempty t  

-- | Constructions the complement of a DFA       
negateMachine t = t { isFinalState = not . isFinalState t }
