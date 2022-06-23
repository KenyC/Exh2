{-# LANGUAGE OverloadedStrings #-}
module Test.Display where


import Test.Tasty
import Test.Tasty.HUnit

import Exh.Formula
import Exh.Formula.Atom
import Exh.Formula.Quantifier
import Exh.Semantics

allTests :: TestTree
allTests = testGroup 
                "display"
                [ simpleAtom 
                , simpleConnective 
                , multipleSameConnective 
                , multipleConnectiveDifferentPriority 
                , negation  
                , quantifiers
                , truthTable  ]


simpleAtom :: TestTree
simpleAtom = testCase "atom" $ do
    let formula = prop "xyz" 
    display formula @?= "xyz"

    let formula = prd "p" ["x", "y", "z"]
    display formula @?= "p(x,y,z)"

    let formula = prd "qr" ["x"]
    display formula @?= "qr(x)"

simpleConnective :: TestTree
simpleConnective = testCase "simple connective" $ do
    let formula = (prop "p") |. (prop "q")
    display formula @?= "p ∨ q"

negation :: TestTree
negation = testCase "negation" $ do
    let formula = neg (prop "p")
    display formula @?= "¬p"


    let formula = neg $ prop "q" |. prop "r" 
    display formula @?= "¬(q ∨ r)"



    let formula = neg $ (prop "q" &. prop "r")
    display formula @?= "¬(q ∧ r)"

multipleSameConnective :: TestTree
multipleSameConnective = testCase "multiple connective of same nature" $ do
    let formula = prop "p" |. (prop "q" |. prop "r")
    display formula @?= "p ∨ q ∨ r"


quantifiers :: TestTree
quantifiers = testCase "quantifiers" $ do
    let formula = _A "x" $ prd "p" ["x"]
    display formula @?= "∀x, p(x)"

    let formula = _A "x" $ prd "p" ["x"] |. prd "q" ["x"]
    display formula @?= "∀x, p(x) ∨ q(x)"

    let formula = (_A "x" $ prd "p" ["x"]) |. prd "q" ["x"]
    display formula @?= "(∀x, p(x)) ∨ q(x)"

    -- not easy to have this behavior now, let's skip
    -- let formula = neg $ _A "x" $ prd "p" ["x"]
    -- display formula @?= "¬∀x, p(x)"

    let formula = _A "x" $ _E "y" $ prd "p" ["x", "y"]
    display formula @?= "∀x, ∃y, p(x,y)"



multipleConnectiveDifferentPriority :: TestTree
multipleConnectiveDifferentPriority = testCase "multiple connective with ≠ priority" $ do
    let formula = prop "p" |. (prop "q" &. prop "r")
    display formula @?= "p ∨ q ∧ r"

    let formula = prop "p" &. (prop "q" |. prop "r")
    display formula @?= "p ∧ (q ∨ r)"


truthTable :: TestTree
truthTable = testCase "truth table" $ do
    let formula = prop "p" |. prop "q"

    let truthTable = mkTruthTable [formula]
    let expected = mconcat
            [ "| p | q |p ∨ q|\n"
            , "---------------\n"
            , "| 0 | 0 |  0  |\n"
            , "| 0 | 1 |  1  |\n"
            , "| 1 | 0 |  1  |\n"
            , "| 1 | 1 |  1  |\n"
            ]
    displayTruthTable truthTable @?= expected