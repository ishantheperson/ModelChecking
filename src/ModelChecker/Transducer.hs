{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module ModelChecker.Transducer where 

import Util 

import Data.Set (Set)
import qualified Data.Set as Set 
import Data.Foldable (find)
import Data.Maybe (fromJust)

data Node a = Node { 
  label :: a
} deriving (Show, Eq, Ord)

-- node: Type of nodes
-- sigma: Type of transition labels (alphabet)
data Transducer node sigma = Transducer {
  states :: Set node,
  alphabet :: Set sigma,
  isFinalState :: node -> Bool,
  isInitialState :: node -> Bool,
  transitionFunction :: (node, sigma) -> node
}

getInitialState t = fromJust $ find (isInitialState t) (states t) 

accepts :: forall a b. Transducer a b -> [b] -> Bool 
accepts t = go (getInitialState t) 
  where go :: a -> [b] -> Bool 
        go s = \case 
          [] -> isFinalState t s 
          x:xs -> go (transitionFunction t (s, x)) xs 

-- | Precondition: t1 and t2 must both have the same alphabet 
productMachine :: Transducer a b -> Transducer c b -> Transducer (a, c) b
productMachine t1 t2 = Transducer states' alphabet' isFinalState' isInitialState' transitionFunction' 
  where states' = Set.cartesianProduct (states t1) (states t2)
        --alphabet' = Set.cartesianProduct (alphabet t1) (alphabet t2)
        alphabet' = alphabet t1 
        isFinalState' (n1, n2) = isFinalState t1 n1 && isFinalState t2 n2 
        isInitialState' (n1, n2) = isInitialState t1 n1 && isInitialState t2 n2 

        transitionFunction' ((n1, n2), e) = 
          (transitionFunction t1 (n1, e), transitionFunction t2 (n2, e))

negateMachine :: Transducer a b -> Transducer a b 
negateMachine t1 = t1 {
  isFinalState = not . isFinalState t1 
}

-- Some small test code 
sampleStates = Set.fromList $ map Node [1..3]
sampleAlphabet = Set.fromList ['a']
sampleIsFinalState (Node i) = i == 3
sampleIsInitialState (Node i) = i == 1 
sampleTransitionFunction = \case 
  (Node 3, _) -> Node 3
  (Node i, _) -> Node $ i + 1 

a :: Transducer (Node Int) Char 
a = Transducer sampleStates sampleAlphabet sampleIsFinalState sampleIsInitialState sampleTransitionFunction

-- example usage: accepts a "aaaaa"