module Sat where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Test.HUnit (Test (..), assertBool, runTestTT, (~:), (~?=))
import Test.QuickCheck

-- | A variable is just a character
newtype Var = Var Char
  deriving (Eq, Ord, Show)

-- | A literal is either a positive or a negative variable
data Lit = Lit {polarity :: Bool, var :: Var} deriving (Eq, Ord, Show)

-- | A clause is a disjunction of a number of literals, again storing
-- each literal in a list.
newtype Clause = Disj {lits :: [Lit]} deriving (Eq, Ord, Show)

-- | An expression in CNF (conjunctive normal form) is a conjunction
-- of clauses. We store these clauses in the conjunction in a list.
newtype CNF = Conj {clauses :: [Clause]} deriving (Eq, Ord, Show)

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

-------------------------------------------------------------------------

-- | Is the literal positive?
isPos :: Lit -> Bool
isPos = polarity

-- | Negate a literal
neg :: Lit -> Lit
neg (Lit b x) = Lit (not b) x

instance Semigroup Clause where
  Disj c1 <> Disj c2 = Disj (c1 <> c2)

instance Monoid Clause where
  mempty = Disj mempty

instance Semigroup CNF where
  Conj c1 <> Conj c2 = Conj (c1 <> c2)

instance Monoid CNF where
  mempty = Conj mempty

instance Enum Var where
  toEnum i = Var (toEnum (i + fromEnum 'A'))
  fromEnum (Var v) = fromEnum v - fromEnum 'A'

-- | A lazy long list of variables
allVars :: [Var]
allVars = [vA ..]

-------------------------------------------------------------------------

-- | The number of times each variable appears in the formula
-- >>> countVars exampleFormula
-- fromList [(Var 'A',2),(Var 'B',2),(Var 'C',2)]
countVars :: CNF -> Map Var Int
countVars = undefined

-- | All of the variables that appear anywhere in the formula, in sorted order
-- >>> vars exampleFormula
-- [Var 'A',Var 'B',Var 'C']
vars :: CNF -> [Var]
vars = undefined