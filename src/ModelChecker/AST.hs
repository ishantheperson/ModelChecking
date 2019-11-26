{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module ModelChecker.AST where 

-- We assume all formulas are in 
-- prenex-normal form, which means
-- that quantifiers do not appear in the matrix
-- i.e. the quantifiers are all "in front" of the formula

-- Example (injectivity): \( \forall a b c. a \to c \land b \to c \implies a = b \)

data Statement = Statement [Quantifier] Matrix deriving Show 

data Quantifier = 
    Forall String -- Forall x ... = Not (exists (Not ...)
  | Exists String 
    deriving Show 

data Matrix = 
    Negation Matrix 
  | And Matrix Matrix 
  | RelatedTo Matrix Matrix -- ^ a -> b
  | Equals Matrix Matrix    -- ^ a == b
  | Variable String 
    deriving Show

convertImplies, convertOr, convertNotEqual :: Matrix -> Matrix -> Matrix 
convertImplies a b = Negation a `convertOr` b 
convertOr a b = Negation (Negation a `And` Negation b)
convertNotEqual a b = Negation (a `Equals` b)

printStatement (Right (Statement qs m)) = concatMap printQuantifier qs ++ printMatrix m
printStatement (Left e) = show e  

printQuantifier = \case 
  Forall s -> "forall " ++ s ++ ". "
  Exists s -> "exists " ++ s ++ ". "

printMatrix = \case 
  Negation (Variable s) -> "~" ++ s 
  Negation m -> "~(" ++ printMatrix m ++ ")"
  And a b -> "(" ++ printMatrix a ++ ") && (" ++ printMatrix b ++ ")"
  RelatedTo a b -> "(" ++ printMatrix a ++ ") -> (" ++ printMatrix b ++ ")"
  Equals a b ->  "(" ++ printMatrix a ++ ") == (" ++ printMatrix b ++ ")"
  Variable s -> s 