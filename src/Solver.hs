module Solver where

import Data.Map (Map)
import qualified Data.Map as Map
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
