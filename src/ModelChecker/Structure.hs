{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
module ModelChecker.Structure where 

import ModelChecker.DFA   
import ModelChecker.AST 

import SampleModel 
import Vector 

import Data.Set (Set)
import qualified Data.Set as Set 

import Control.Monad.State 
import Control.Monad.Writer 

data Structure a b c sigma = Structure {
  binOp :: Maybe (DFA a sigma (Succ (Succ Zero))),
  ternaryOp :: Maybe (DFA b sigma (Succ (Succ (Succ Zero)))),
  equalOp :: DFA c sigma (Succ (Succ Zero))
}

type Context m = (MonadState (Set String) m, MonadWriter [String] m)

-- | Takes a structure and a statement, and returns a list of errors encountered
checkStatement structure (Statement qs m) = 
  execWriter $ execStateT (forM_ qs checkQuant >> checkAST m) Set.empty 
  where checkQuant :: Context m => Quantifier -> m () 
        checkQuant s = do 
          let name = case s of { Forall s -> s; Exists s -> s }

          seen <- gets $ Set.member name 
          when seen $ tell ["variable defined multiple times: '" ++ name ++ "'"]
          modify $ Set.insert name 

        checkVar :: Context m => String -> m () 
        checkVar s = do 
          exists <- gets $ Set.member s 
          unless exists $ tell ["unquantified variable '" ++ s ++ "'"]

        checkAST :: Context m => Matrix -> m () 
        checkAST = \case 
          Negation m -> checkAST m 
          And m1 m2 -> checkAST m1 >> checkAST m2 
          RelatedTo a b -> case binOp structure of 
            Nothing -> tell ["binary operation not supported"]
            Just _ -> checkVar a >> checkVar b 

          TernaryOp a b c -> case ternaryOp structure of 
            Nothing -> tell ["ternary operation not supported"]
            Just _ -> forM_ [a, b, c] checkVar

          Equals a b -> checkVar a >> checkVar b 

--presburger :: Structure 
presburger = Structure { binOp = Just succDFA, ternaryOp = Just addDFA, equalOp = eqDFA }