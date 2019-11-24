{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module Util where 
  
import Control.Applicative
import Control.Monad  
import "template-haskell" Language.Haskell.TH

data Nat = Zero | Succ Nat deriving (Show, Eq, Ord)
data SNat n where 
  SZero :: SNat Zero 
  SSucc :: SNat n -> SNat (Succ n)  

deriving instance Show (SNat a)   

data Vector n a where 
  VEmpty :: Vector Zero a
  (:+)   :: a -> Vector n a -> Vector (Succ n) a 
  
deriving instance Show a => Show (Vector n a)

infixr 5 :+

-- These mean we can only compare
-- vectors of the same length :O 
instance Eq a => Eq (Vector n a) where 
  (==) VEmpty VEmpty = True 
  (==) (x :+ xs) (y :+ ys) = x == y && xs == ys 

instance Ord a => Ord (Vector n a) where 
  compare VEmpty VEmpty = EQ 
  compare (x :+ xs) (y :+ ys) = 
    case compare x y of 
      EQ -> compare xs ys 
      other -> other 

data Finite :: Nat -> * where
  FZero :: Finite (Succ n) 
  FSucc :: Finite n -> Finite (Succ n) 

index :: Finite n -> Vector n a -> a
index FZero (x :+ _) = x
index (FSucc i) (_ :+ xs) = index i xs  

prepend :: a -> Vector n a -> Vector (Succ n) a 
prepend a xs = a :+ xs 

append :: a -> Vector n a -> Vector (Succ n) a 
append a VEmpty = a :+ VEmpty
append a (x :+ xs) = x :+ append a xs

deleteFirst :: Vector (Succ n) a -> Vector n a 
deleteFirst (x :+ xs) = xs 

deleteLast :: Vector (Succ n) a -> Vector n a 
deleteLast (x :+ VEmpty) = VEmpty
deleteLast (x :+ (y :+ ys)) = x :+ deleteLast (y :+ ys)

{-
type family LessThan (a :: Nat) (b :: Nat) = c where 
  LessThan Zero b = True 
  LessThan (Succ a) (Succ b) = LessThan a b 
  LessThan a Zero = False -- TypeError (Text "a must be less than b")

-- index :: forall i n a. (LessThan i n ~ True) => Vector n a -> SNat n -> a 
index (a :+ b) SZero = a 
index (a :+ b) (SSucc x) = index b x 
-}

-- | This function gets all possible vector combinations
--   where the elements come from a bounded enumeration 
--   of length n 
getAlphabet :: (Bounded a, Enum a) => SNat n -> [Vector n a]
getAlphabet = getAllVectors [minBound..maxBound] 

getAllVectors :: [a] -> SNat n -> [Vector n a]
getAllVectors s SZero = [VEmpty]
getAllVectors s (SSucc x) = (:+) <$> s <*> getAllVectors s x  

fromVec2 :: Vector (Succ (Succ Zero)) a -> (a, a)
fromVec2 (a :+ b :+ VEmpty) = (a, b)
toVec2 (a, b) = a :+ b :+ VEmpty 

fromVec3 :: Vector (Succ (Succ (Succ Zero))) a -> (a, a, a)
fromVec3 (a :+ b :+ c :+ VEmpty)  = (a, b, c)

toVec3 (a, b, c) = a :+ b :+ c :+ VEmpty

mkSnat :: Int -> Q Exp
mkSnat 0 = [| SZero |]
mkSnat i | i > 0 = [| SSucc $(mkSnat $ pred i) |]
mkSnat other = error $ "mkSnat " ++ show other ++ ": must be nonnegative"

{-
data Nat1 = Zero | Succ Nat1

type family NSucc  (n :: Nat) = (c :: Nat) | c -> n where 
  NSucc i = 1 + i

type family FromNat1 (n :: Nat1) = (c :: Nat) -- | c -> n 
type instance FromNat1 (Zero)     = 0
type instance FromNat1 (Succ n) = 1 + FromNat1 n

getAllVectors :: forall s n. KnownNat n => [s] -> nq -> [Vector n s]
getAllVectors s 0 = [Vec.empty]
getAllVectors s x = Vec.cons <$> s <*> getAllVectors s (x - 1)

-- type instance FromNat1 Zero     = 0
-- type instance FromNat1 (Succ n) = 1 + FromNat1 n

class Go (n :: Nat1) where 
  go :: [a] -> Nat1 -> [Vector (FromNat1 n) a]

instance Go Zero where 
  go s Zero = [Vec.empty]

instance Go n => Go (Succ n) where 
  go s (Succ n) = getAllVectors s (go s n)

getAllVectors :: forall a b. [a] -> [Vector b a] -> [Vector (1 + b) a]
getAllVectors s vs = Vec.cons <$> s <*> vs 
  -}

{-
go :: forall a b. [a] -> b -> [Vector b a]
go s Proxy  = [Vec.empty]
go s (Succ x) = getAllVectors s (go s x)

getAllVectors :: forall a b. [a] -> [Vector b a] -> [Vector (1 + b) a]
getAllVectors s vs = Vec.cons <$> s <*> vs 
-}
-- getAllVectors s n : gets all vectors of size n with elements in s 
--getAllVectors :: Foldable f => f a -> b -> [Vector b a]
{-
getAllVectors :: forall s n. [s] -> Nat1 -> [Vector (FromNat1 n) s]
getAllVectors s Zero = [Vec.empty]
getAllVectors s (Succ x) = Vec.cons <$> s <*> getAllVectors s x
-}
