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

main :: IO ()
main = do
  putStrLn "Unit tests:"
  _ <- runTestTT $ TestList [testCountVars, testVars, testGenVars, testGenCNF, testSatisfiedBy]
  putStrLn "Run Finished"