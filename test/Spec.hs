import qualified Data.List as List
import qualified Data.Map as Map
import Formula
import Lib ()
import Sat
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

main :: IO ()
main = do
  putStrLn "Unit tests:"
  _ <- runTestTT $ TestList [testCountVars, testVars, testGenVars, testGenCNF, testSatisfiedBy]
  putStrLn "QuickCheck properties:"
  putStrLn "prop_unSatBy"
  quickCheckN 500 prop_unSatBy