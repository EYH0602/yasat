module Solver (Solver, makeValuations, sat0, instantiate) where

import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import Formula
import Sat
import Valuation

type Solver = CNF -> Maybe Valuation

makeValuations :: [Var] -> [Valuation]
makeValuations vs = map (Map.fromList . zip vs) (generate (length vs))
  where
    generate :: Int -> [[Bool]] -- generate all possible combinations of Bool, 2^n
    generate 0 = [[]]
    generate n = [x : xs | x <- [True, False], xs <- generate (n - 1)]

sat0 :: Solver
sat0 cnf = find (satisfiedBy cnf) valuations
  where
    valuations = makeValuations $ vars cnf

instantiateClause :: Var -> Bool -> [Lit] -> Maybe [Lit]
instantiateClause v p ls
  | any evaluatesToTrue ls = Nothing -- clause is satisfied; remove it
  | otherwise =
    let ls' = filter (not . evaluatesToFalse) ls
     in if null ls'
          then Just [] -- clause is unsatisfiable
          else Just ls'
  where
    evaluatesToTrue (Lit p' v')
      | v /= v' = False
      | otherwise = p == p'
    evaluatesToFalse (Lit p' v')
      | v /= v' = False
      | otherwise = p /= p'

instantiate :: CNF -> Var -> Bool -> CNF
instantiate cnf v p
  | v `notElem` vars cnf = cnf
  | otherwise =
    let processedClauses = map (instantiateClause v p . lits) (clauses cnf)
     in if any isUnsat processedClauses
          then Conj [Disj []] -- The CNF is unsatisfiable
          else Conj $ map Disj $ catMaybes processedClauses
  where
    isUnsat (Just []) = True
    isUnsat _ = False
