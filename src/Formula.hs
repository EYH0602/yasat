module Formula where

import Control.Monad (liftM2, liftM3)
import Sat
import Test.QuickCheck

-- A few variables for test cases
vA, vB, vC, vD :: Var
vA = Var 'A'
vB = Var 'B'
vC = Var 'C'
vD = Var 'D'

exampleFormula :: CNF
exampleFormula =
  Conj
    [ Disj [Lit True vA, Lit True vB, Lit True vC],
      Disj [Lit False vA],
      Disj [Lit False vB, Lit True vC]
    ]

-- | A lazy long list of variables
allVars :: [Var]
allVars = [vA ..]

-- | Generate a random variable (limited to the first `n` variables).
genVar :: Int -> Gen Var
genVar n | n < 1 = error "Must supply a positive number to genVar"
genVar n = elements $ take n allVars

-- | Generate a random literal with `n` distinct variables.
genLit :: Int -> Gen Lit
genLit n = do
  p <- choose (True, False)
  v <- genVar n
  return $ Lit p v

-- | Generate a random Clause with `n` distinct variables.
genClause :: Int -> Gen Clause
genClause n = do
  ls <- listOf $ genLit n
  return $ Disj ls

-- | Generate a random CNF with `n` distinct variables.
genCNF :: Int -> Gen CNF
genCNF n = do
  cs <- listOf $ genClause n
  return $ Conj cs

defaultNumVariables :: Int
defaultNumVariables = 7

instance Arbitrary Var where
  arbitrary = genVar defaultNumVariables
  shrink v
    | v == vA = []
    | otherwise = [vA .. pred v]

instance Arbitrary Lit where
  arbitrary = genLit defaultNumVariables
  shrink (Lit b v) =
    map (`Lit` v) (shrink b)
      ++ map (Lit b) (shrink v)

instance Arbitrary Clause where
  arbitrary = genClause defaultNumVariables
  shrink (Disj l) = map Disj (shrink l)

instance Arbitrary CNF where
  arbitrary = genCNF defaultNumVariables
  shrink (Conj x) = map Conj (shrink x)