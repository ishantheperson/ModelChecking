{-# LANGUAGE LambdaCase, ScopedTypeVariables, FlexibleContexts #-}
module ModelChecker.Transducer where 

import Util 

import Data.Set (Set, (\\))
import qualified Data.Set as Set 
import Data.Foldable (find)
import Data.Maybe (fromJust)

import Control.Monad 
import Control.Monad.State 

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

getDestinations :: Ord a => Transducer a b -> a -> Set a 
getDestinations t n = Set.map (curry (transitionFunction t) n) (alphabet t)

accepts :: forall a b. Transducer a b -> [b] -> Bool 
accepts t = go $ getInitialState t
  where go :: a -> [b] -> Bool 
        go s = \case 
          [] -> isFinalState t s 
          x:xs -> go (transitionFunction t (s, x)) xs 

empty :: forall a b. Ord a => Transducer a b -> Bool 
empty t = Set.null . Set.filter (isFinalState t) $ reachable
  where go :: MonadState (Set a) m => a -> [a] -> m ()
        go currentNode next = do 
          visited <- get 
          if Set.member currentNode visited 
            then return () 
            else do 
              modify $ Set.insert currentNode 
              let destinitions = getDestinations t currentNode \\ visited 
              case Set.toList destinitions ++ next of 
                [] -> return () 
                x:xs -> go x xs 

        reachable :: Set a 
        reachable = execState (go (getInitialState t) []) Set.empty 

-- | Precondition: t1 and t2 must both have the same alphabet 
productMachine :: Transducer a b -> Transducer c b -> Transducer (a, c) b
productMachine t1 t2 = Transducer states' alphabet' isFinalState' isInitialState' transitionFunction' 
  where states' = Set.cartesianProduct (states t1) (states t2)
        alphabet' = alphabet t1 
        isFinalState' (n1, n2) = isFinalState t1 n1 && isFinalState t2 n2 
        isInitialState' (n1, n2) = isInitialState t1 n1 && isInitialState t2 n2 

        transitionFunction' ((n1, n2), e) = 
          (transitionFunction t1 (n1, e), transitionFunction t2 (n2, e))

negateMachine :: Transducer a b -> Transducer a b 
negateMachine t1 = t1 { isFinalState = not . isFinalState t1 }

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

-- example usage: accepts a "aaaaa"