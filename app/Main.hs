module Main (main) where

import qualified Data.Map as Map
import Lib ()
import Sat (CNF (..), Clause (..), Lit (..), Var (..), countVars, vars)
import Test.HUnit (Counts, Test (..), assertBool, runTestTT, (~:), (~?=))

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

main :: IO ()
main = do
  putStrLn "Unit tests:"
  _ <- runTestTT $ TestList [testCountVars, testVars]
  putStrLn "Run Finished"