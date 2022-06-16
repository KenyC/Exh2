{-# LANGUAGE OverloadedStrings #-}
module Test.Display where


import Test.Tasty
import Test.Tasty.HUnit

import Exh.Formula
import Exh.Formula.Atom
import Exh.Semantics

allTests :: TestTree
allTests = testGroup 
                "display"
                [ simpleAtom 
                , simpleConnective 
                , multipleSameConnective 
                , multipleConnectiveDifferentPriority 
                , negation  
                , truthTable  ]


simpleAtom :: TestTree
simpleAtom = testCase "atom" $ do
    let formula = atom "xyz" 
    display formula @?= "xyz"

simpleConnective :: TestTree
simpleConnective = testCase "simple connective" $ do
    let formula = (atom "p") |. (atom "q")
    display formula @?= "p ∨ q"

negation :: TestTree
negation = testCase "negation" $ do
    let formula = neg (atom "p")
    display formula @?= "¬p"


    let formula = neg $ atom "q" |. atom "r" 
    display formula @?= "¬(q ∨ r)"



    let formula = neg $ (atom "q" &. atom "r")
    display formula @?= "¬(q ∧ r)"

multipleSameConnective :: TestTree
multipleSameConnective = testCase "multiple connective of same nature" $ do
    let formula = atom "p" |. (atom "q" |. atom "r")
    display formula @?= "p ∨ q ∨ r"



multipleConnectiveDifferentPriority :: TestTree
multipleConnectiveDifferentPriority = testCase "multiple connective with ≠ priority" $ do
    let formula = atom "p" |. (atom "q" &. atom "r")
    display formula @?= "p ∨ q ∧ r"

    let formula = atom "p" &. (atom "q" |. atom "r")
    display formula @?= "p ∧ (q ∨ r)"


truthTable :: TestTree
truthTable = testCase "truth table" $ do
    let formula = atom "p" |. atom "q"

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