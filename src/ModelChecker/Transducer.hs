{-# LANGUAGE LambdaCase #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module ModelChecker.Transducer where 

import Util 

import Data.Set (Set, (\\))
import qualified Data.Set as Set 
import Data.Foldable (find)

import Control.Monad.State 

-- Honestly this is probably not necessary 
newtype Node a = Node { label :: a } deriving (Show, Eq, Ord)

-- Right now represents a deterministic
-- transducer. You could (inefficiently)
-- use this to 
-- node: Type of nodes
-- sigma: Type of transition labels (alphabet)
data Transducer node sigma arity = Transducer {
  states :: Set node,
  --alphabet :: Set sigma,
  arity :: SNat arity,
  isFinalState :: node -> Bool,
  isInitialState :: node -> Bool,
  transitionFunction :: (node, Vector arity sigma) -> node
}

getInitialState t = 
  case find (isInitialState t) (states t) of 
    Just s -> s 
    Nothing -> error "No initial state found in given automata!"

--getDestinations :: Ord a => Transducer a b c -> Vector c a -> Set a 
getDestinations :: (Ord node, Ord sigma, Bounded sigma, Enum sigma) => 
                      Transducer node sigma arity -> node -> Set node 
getDestinations t n = 
  Set.map (curry (transitionFunction t) n) (Set.fromList $ getAlphabet (arity t))

accepts :: forall node sigma arity. Transducer node sigma arity -> [Vector arity sigma] -> Bool 
accepts t = go $ getInitialState t
  where go :: node -> [Vector arity sigma] -> Bool
        go s = \case 
          [] -> isFinalState t s 
          x:xs -> go (transitionFunction t (s, x)) xs 
         
empty :: forall node sigma arity. (Ord node, Ord sigma, Bounded sigma, Enum sigma) => 
              Transducer node sigma arity -> Bool
empty t = Set.null . Set.filter (isFinalState t) $ reachable
  where dfs :: MonadState (Set node) m => node -> [node] -> m ()
        dfs currentNode next = do 
          visited <- get 

          if Set.member currentNode visited 
            then return ()
            else do 
              modify $ Set.insert currentNode 

              let destinations = getDestinations t currentNode \\ visited 
              case Set.toList destinations ++ next of 
                [] -> return () 
                x:xs -> dfs x xs 

        reachable :: Set node
        reachable = execState (dfs (getInitialState t) []) Set.empty   

productMachine :: Transducer n1 b c -> Transducer n2 b c -> Transducer (n1, n2) b c
productMachine t1 t2 = Transducer states' arity' isFinalState' isInitialState' transitionFunction' 
  where states' = states t1 `Set.cartesianProduct` states t2
        arity' = arity t1
        isFinalState' (n1, n2) = isFinalState t1 n1 && isFinalState t2 n2 
        isInitialState' (n1, n2) = isInitialState t1 n1 && isInitialState t2 n2 

        transitionFunction' ((n1, n2), e) = 
          (transitionFunction t1 (n1, e), transitionFunction t2 (n2, e))

negateMachine :: Transducer a b c -> Transducer a b c
negateMachine t1 = t1 { isFinalState = not . isFinalState t1 }

{-



-- Some small test code   

sampleStates = Set.fromList $ map Node [1..3]
sampleAlphabet = Set.fromList ['a']
sampleIsFinalState (Node i) = i == 3
sampleIsInitialState (Node i) = i == 1 
sampleTransitionFunction = \case 
  (Node 3, _) -> Node 3
  (Node i, _) -> Node $ i + 1 

-- L(a) = a^n where n >= 2
a :: Transducer (Node Int) Char 
a = Transducer sampleStates sampleAlphabet sampleIsFinalState sampleIsInitialState sampleTransitionFunction

-- L(b) = { [], a }
b = negateMachine a 

-- L(c) = {} 
-- i.e. empty c == True 
c = a `productMachine` b 
-- example usage: accepts a "aaaaa"

sampleTransitionFunction2 = \case 
  (Node 3, _) -> Node 1
  (Node i, _) -> Node $ i + 1
d = Transducer sampleStates sampleAlphabet sampleIsFinalState sampleIsInitialState sampleTransitionFunction2
-}