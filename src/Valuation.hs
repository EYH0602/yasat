module Valuation where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Formula
import Sat
import Test.QuickCheck

-- | Assignments of values to (some) variables
type Valuation = Map Var Bool

emptyValuation :: Valuation
emptyValuation = Map.empty

fromList :: [(Var, Bool)] -> Valuation
fromList = Map.fromList

exampleValuation :: Valuation
exampleValuation = Map.fromList [(vA, False), (vB, True), (vC, True)]

litSatisfied :: Valuation -> Lit -> Bool
litSatisfied a lit = a Map.!? var lit == Just (polarity lit)

satisfiedBy :: CNF -> Valuation -> Bool
satisfiedBy p a = all (any (litSatisfied a) . lits) (clauses p)