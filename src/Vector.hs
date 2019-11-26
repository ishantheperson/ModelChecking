{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}

module Vector where 
  
import Control.Applicative  
import "template-haskell" Language.Haskell.TH

-- | Represents a natural number as a type
data Nat = Zero | Succ Nat deriving (Show, Eq, Ord)

-- TODO: injectivity? 
type family (n :: Nat) + (m :: Nat) = (c :: Nat) where 
  Zero     + m = m 
  (Succ n) + m = Succ (n + m)

addSnat :: SNat n -> SNat m -> SNat (n + m)
addSnat SZero m = m 
addSnat (SSucc n) m = SSucc (n `addSnat` m)

-- | Connects the concrete (value level) representation
--   of a natural number with the type level representation
--   above. 
data SNat n where 
  SZero :: SNat Zero 
  SSucc :: SNat n -> SNat (Succ n)  

deriving instance Show (SNat a)   

data LessThanEqual :: Nat -> Nat -> * where 
  LTEZero :: LessThanEqual Zero m 
  LTESucc :: LessThanEqual n m -> LessThanEqual (Succ n) (Succ m)

deriving instance Show (LessThanEqual a b)  

data Void 
type Refuted a = (a -> Void)
data Decision a = Proved a   
                | Disproved (Refuted a)

lessThanEqual :: SNat n -> SNat m -> Decision (LessThanEqual n m)
lessThanEqual SZero _ = Proved LTEZero 
lessThanEqual (SSucc n) SZero = Disproved (\case { })
lessThanEqual (SSucc n) (SSucc m) = 
  case lessThanEqual n m of 
    Proved l -> Proved $ LTESucc l 
    Disproved p -> Disproved $ \case LTESucc l -> p l 

atLeast :: SNat n -> Vector m a -> Maybe (LessThanEqual n m, Vector m a)    
--atLeast :: SNat n -> Vector m a -> Maybe (LessThanEqual n m, Vector (n + k) a)
atLeast n v = case lessThanEqual n (vlength v) of 

-- | Represents a vector parameterized by its 
--   length as well as its data type. 
data Vector n a where 
  VEmpty :: Vector Zero a
  (:+)   :: a -> Vector n a -> Vector (Succ n) a 
  
deriving instance Show a => Show (Vector n a)

instance Functor (Vector n) where 
  fmap _ VEmpty    = VEmpty
  fmap f (x :+ xs) = f x :+ fmap f xs

instance Foldable (Vector n) where   
  foldMap _ VEmpty = mempty 
  foldMap f (x :+ xs) = f x <> foldMap f xs 

infixr 5 :+

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

withVector2 :: [a] -> (forall (n :: Nat). SNat (Succ (Succ n)) -> Vector (Succ (Succ n)) a -> b) -> b
withVector2 (x:y:[]) f = f (SSucc (SSucc SZero)) $ x :+ y :+ VEmpty
withVector2 (x:y:ys) f = withVector2 (y:ys) $ \len vs -> f (SSucc len) (x :+ vs)

-- | Creates a new vector of the given size using a function 
newWith :: SNat n -> (Finite n -> a) -> Vector n a
newWith SZero     _ = VEmpty
newWith (SSucc i) f = f FZero :+ newWith i (f . FSucc)

vlength :: Vector n a -> SNat n
vlength VEmpty = SZero 
vlength (_ :+ xs) = SSucc (vlength xs)

-- | Gets the index of an element in a vector. Crashes if it does not exist 
indexOf :: Eq a => Vector n a -> a -> (Finite n -> b) -> b 
indexOf VEmpty    _ _ = error "Cannot find"
indexOf (x :+ xs) a f = if a == x 
                          then f FZero 
                          else indexOf xs a (f . FSucc)

-- | Indexing into a vector (poor performance)
--   but is safe. If we cared about performance
--   we wouldn't have written the project in a functional
--   programming language lmao. 
index :: Vector n a -> Finite n -> a
index (x :+ _)  FZero     = x
index (_ :+ xs) (FSucc i) = index xs i

-- | Updates the given vector at a position. 
update :: Vector n a -> a -> Finite n -> Vector n a 
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
getAllVectors s SZero     = [VEmpty]
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
