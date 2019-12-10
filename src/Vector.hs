{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE BangPatterns #-}

module Vector where 

import Data.Foldable (toList)
import "template-haskell" Language.Haskell.TH

-- | Represents a natural number as a type
data Nat = Zero | Succ Nat deriving (Show, Eq, Ord)

-- | Connects the concrete (value level) representation
--   of a natural number with the type level representation
--   above. 
data SNat n where 
  SZero :: SNat Zero 
  SSucc :: SNat n -> SNat (Succ n)  

deriving instance Show (SNat a)   

-- | Represents a vector parameterized by its 
--   length as well as its data type. 
data Vector n a where 
  VEmpty :: Vector Zero a
  (:+)   :: a -> Vector n a -> Vector (Succ n) a 

instance Functor (Vector n) where 
  fmap _ VEmpty    = VEmpty
  fmap f (x :+ xs) = f x :+ fmap f xs

instance Foldable (Vector n) where   
  foldMap _ VEmpty = mempty 
  foldMap f (x :+ xs) = f x <> foldMap f xs 

infixr 5 :+

instance Traversable (Vector n) where 
  traverse _ VEmpty = pure VEmpty
  traverse f (x :+ xs) = (:+) <$> f x <*> traverse f xs 

instance Show a => Show (Vector n a) where 
  show v = "{" ++ (show $ toList v) ++ "}"


-- These mean we can only compare
-- vectors of the same length :O 
instance Eq a => Eq (Vector n a) where 
  (==) VEmpty    VEmpty    = True 
  (==) (x :+ xs) (y :+ ys) = x == y && xs == ys 

instance Ord a => Ord (Vector n a) where 
  compare VEmpty    VEmpty    = EQ 
  compare (x :+ xs) (y :+ ys) = 
    case compare x y of 
      EQ    -> compare xs ys 
      other -> other 

-- | The type @Finite n@ has @n@ many 
--   elements, so it is useful for vector
--   indices 
data Finite :: Nat -> * where
  FZero :: Finite (Succ n) 
  FSucc :: Finite n -> Finite (Succ n) 

deriving instance Show (Finite n)

instance Bounded (Finite (Succ Zero)) where 
  minBound = FZero  
  maxBound = FZero 

instance Bounded (Finite (Succ n)) => Bounded (Finite (Succ (Succ n))) where 
  minBound = FZero 
  maxBound = FSucc (maxBound :: Finite (Succ n)) 

snatToFinite :: SNat (Succ n) -> Finite (Succ n) 
snatToFinite (SSucc SZero) = FZero 
snatToFinite (SSucc (SSucc n)) = FSucc (snatToFinite (SSucc n))

new :: SNat n -> a -> Vector n a 
new SZero     _ = VEmpty
new (SSucc i) a = a :+ new i a 

-- | Converts a list into a sized vector using CPS 
withVector :: [a] -> (forall (n :: Nat). SNat n -> Vector n a -> b) -> b 
withVector []     f = f SZero VEmpty 
withVector (x:xs) f = withVector xs $ \len vs -> f (SSucc len) (x :+ vs)  

-- | Creates a new vector of the given size using a function 
newWith :: SNat n -> (Finite n -> a) -> Vector n a
newWith SZero     _ = VEmpty
newWith (SSucc i) f = f FZero :+ newWith i (f . FSucc)

vlength :: Vector n a -> SNat n
vlength VEmpty = SZero 
vlength (_ :+ xs) = SSucc (vlength xs)

-- | Gets the index of an element in a vector. Crashes if it does not exist 
indexOf' :: Eq a => Vector n a -> a -> (Finite n -> b) -> b 
indexOf' VEmpty    _ _ = error "Cannot find"
indexOf' (x :+ xs) a f = if a == x 
                          then f FZero 
                          else indexOf' xs a (f . FSucc)

indexOf v x = indexOf' v x id 

withIndices :: Vector n a -> b -> (Finite n -> a -> b -> b) -> b 
withIndices VEmpty b _ = b 
withIndices (x :+ VEmpty) b f = f FZero x b 
withIndices (x :+ xs) b f = withIndices xs (f FZero x b) $ 
  \i e accum -> f (FSucc i) e accum 
                          
-- | O(n) safe indexing into a vector (given valuable arguments)
index :: Vector n a -> Finite n -> a
index VEmpty    !(_)       = undefined -- Only reachable by passing ⊥ as index
index (x :+ _)  !(FZero)   = x
index (_ :+ xs) !(FSucc i) = index xs i

-- | Updates the given vector at a position. 
update :: Vector n a -> a -> Finite n -> Vector n a 
update VEmpty    _ _         = VEmpty
update (_ :+ xs) x FZero     = x :+ xs 
update (y :+ xs) x (FSucc i) = y :+ update xs x i 

-- | Cons onto the front of a vector
prepend :: a -> Vector n a -> Vector (Succ n) a 
prepend a xs = a :+ xs 

-- | Add to the end of a vector
append :: a -> Vector n a -> Vector (Succ n) a 
append a VEmpty    = a :+ VEmpty
append a (x :+ xs) = x :+ append a xs

-- | Removes the first element of this vector 
deleteFirst :: Vector (Succ n) a -> Vector n a 
deleteFirst (_ :+ xs) = xs 

-- | Removes the last element of this vector 
deleteLast :: Vector (Succ n) a -> Vector n a 
deleteLast (_ :+ VEmpty)    = VEmpty
deleteLast (x :+ (y :+ ys)) = x :+ deleteLast (y :+ ys)

-- | This function gets all possible vector combinations
--   where the elements come from a bounded enumeration 
--   of length n 
getAlphabet :: (Bounded a, Enum a) => SNat n -> [Vector n a]
getAlphabet = getAllVectors [minBound..maxBound] 

getAllVectors :: [a] -> SNat n -> [Vector n a]
getAllVectors _ SZero     = [VEmpty]
getAllVectors s (SSucc x) = (:+) <$> s <*> getAllVectors s x  

fromVec2 :: Vector (Succ (Succ Zero)) a -> (a, a)
fromVec2 (a :+ b :+ VEmpty) = (a, b)
toVec2 (a, b) = a :+ b :+ VEmpty 

fromVec3 :: Vector (Succ (Succ (Succ Zero))) a -> (a, a, a)
fromVec3 (a :+ b :+ c :+ VEmpty)  = (a, b, c)
toVec3 (a, b, c) = a :+ b :+ c :+ VEmpty

toVec4 (a, b, c, d) = a :+ b :+ c :+ d :+ VEmpty 

-- | Makes the singleton instance of a natural number
--   from an integer literal 
mkSnat :: Int -> Q Exp
mkSnat 0         = [| SZero |]
mkSnat i | i > 0 = [| SSucc $(mkSnat $ pred i) |]
mkSnat other     = error $ "mkSnat " ++ show other ++ ": must be nonnegative"

-- | Makes the instance of the finite type corresponding
--   to the integer literal 
mkFinite :: Int -> Q Exp 
mkFinite 0         = [| FZero |]
mkFinite i | i > 0 = [| FSucc $(mkFinite $ pred i) |]
mkFinite other     = error $ "mkFinite " ++ show other ++ ": must be nonnegative"
