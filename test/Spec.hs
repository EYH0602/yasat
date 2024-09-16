{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

import qualified Data.List as List
import qualified Data.Map as Map
import Formula
import Lib ()
import Sat
import Solver
import Test.HUnit (Counts, Test (..), assertBool, runTestTT, (~:), (~?=))
import Test.QuickCheck
import Valuation

testCountVars :: Test
testCountVars =
  "countVars"
    ~: countVars exampleFormula
    ~?= Map.fromList [(vA, 2), (vB, 2), (vC, 2)]

testVars :: Test
testVars =
  "vars"
    ~: vars exampleFormula
    ~?= [vA, vB, vC]

-------------------------------------------------------------------------

-- | Test cases for Formula generators
-- make sure that genVars produces the right number of variables.
testGenVars :: Test
testGenVars =
  "genVar" ~: do
    xs <- sample' (genVar 3)
    return $ length (List.nub xs) == 3

-- make sure that arbitrary formulae don't contain too many variables.
testGenCNF :: Test
testGenCNF =
  "genCNF" ~: do
    xs <- sample' (genCNF defaultNumVariables)
    return $ all (\c -> length (countVars c) <= defaultNumVariables) xs

testSatisfiedBy :: Test
testSatisfiedBy =
  "satisfiedBy"
    ~: TestList
      [ "exampleFormula"
          ~: assertBool "" (exampleFormula `satisfiedBy` exampleValuation),
        "should filed valuation"
          ~: assertBool "" (not (exampleFormula `satisfiedBy` valuation')),
        "empty bur valid"
          ~: assertBool "" (validEmptyFormula `satisfiedBy` exampleValuation),
        "disjunction"
          ~: assertBool "" (not (anotherUnsatFormula `satisfiedBy` exampleValuation))
      ]
  where
    valuation' = Map.fromList [(vA, False), (vB, False), (vC, False)]
    validEmptyFormula = Conj []
    anotherUnsatFormula = Conj [Disj []]

--  A /\ (not A), shot not be satisfied by any valuation
unSatFormula :: CNF
unSatFormula = Conj [Disj [Lit True vA], Disj [Lit False vA]]

prop_unSatBy :: Valuation -> Property
prop_unSatBy v = property (not (unSatFormula `satisfiedBy` v))

--------------------------------------------------------------------------
quickCheckN :: (Testable prop) => Int -> prop -> IO ()
quickCheckN n = quickCheckWith $ stdArgs {maxSuccess = n}

--------------------------------------------------------------------------
------- Tests for Solver -----

allElementsDistinct :: (Eq a) => [a] -> Bool
allElementsDistinct [] = True
allElementsDistinct (x : xs) =
  x `notElem` xs
    && allElementsDistinct xs

prop_makeValuations :: CNF -> Property
prop_makeValuations p =
  length valuations === 2 ^ length ss
    .&&. allElementsDistinct valuations
  where
    valuations = makeValuations ss :: [Valuation]
    ss = vars p

prop_satResultSound :: Solver -> CNF -> Property
prop_satResultSound solver p = case solver p of
  Just a -> collect "sat" $ p `satisfiedBy` a
  Nothing -> collect "unsat" $ property True

-- a formula is unsatisfiable when there is no satisfying valuation
-- out of all of the possible assignments of variables to truth values.
unsatisfiable :: CNF -> Bool
unsatisfiable p = not . any (p `satisfiedBy`) $ makeValuations (vars p)

prop_satResultCorrect :: Solver -> CNF -> Property
prop_satResultCorrect solver p = property $ case solver p of
  Just a -> p `satisfiedBy` a
  Nothing -> unsatisfiable p

main :: IO ()
main = do
  putStrLn "Unit tests:"
  _ <- runTestTT $ TestList [testCountVars, testVars, testGenVars, testGenCNF, testSatisfiedBy]
  putStrLn "QuickCheck properties:"
  putStrLn "prop_unSatBy"
  quickCheckN 500 prop_unSatBy
  putStrLn "prop_makeValuations"
  quickCheckN 500 prop_makeValuations
  putStrLn "prop_satResultSound"
  quickCheckN 500 (prop_satResultSound sat0)
  putStrLn "prop_satResultCorrect"
  quickCheckN 500 (prop_satResultCorrect sat0)
