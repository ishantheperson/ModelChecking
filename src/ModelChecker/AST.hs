{-# LANGUAGE FlexibleContexts #-}
module ModelChecker.AST where 

-- We assume all formulas are in 
-- prenex-normal form, which means
-- that quantifiers do not appear in the matrix
-- i.e. the quantifiers are all "in front" of the formula

data Statement = Statement [Quantifier] Matrix deriving Show 

data Quantifier = 
    Forall String 
  -- | Exists String 
    deriving Show 

data Matrix = 
    Negation Matrix 
  | And Matrix Matrix 
  | RelatedTo Matrix Matrix 
  | Equals Matrix Matrix 
  | Variable String 
    deriving Show

convertExists = error "Exists not supported yet"

convertOr :: Matrix -> Matrix -> Matrix 
convertOr a b = Negation (Negation a `And` Negation b)

convertNotEqual :: Matrix -> Matrix -> Matrix 
convertNotEqual a b = Negation (a `Equals` b)

