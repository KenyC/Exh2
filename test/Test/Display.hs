{-# LANGUAGE OverloadedStrings #-}
module Test.Display where


import Test.Tasty
import Test.Tasty.HUnit

import Exh.Formula

allTests :: TestTree
allTests = testGroup 
                "display"
                [ simpleAtom 
                , simpleConnective 
                , multipleSameConnective 
                , multipleConnectiveDifferentPriority 
                , negation  ]


simpleAtom :: TestTree
simpleAtom = testCase "atom" $ do
    let formula = Atom "xyz" 
    display formula @?= "xyz"

simpleConnective :: TestTree
simpleConnective = testCase "simple connective" $ do
    let formula = 
            Op @Or
                (MkF $ Atom "p") 
                (MkF $ Atom "q") 

    display formula @?= "p ∨ q"

negation :: TestTree
negation = testCase "negation" $ do
    let formula = 
            Neg (MkF $ Atom "p") 

    display formula @?= "¬p"

    let formula = 
            Neg $  
            MkF $ Op @Or
                (MkF $ Atom "q") 
                (MkF $ Atom "r") 


    display formula @?= "¬(q ∨ r)"

    let formula = 
            Neg $  
            MkF $ Op @And
                (MkF $ Atom "q") 
                (MkF $ Atom "r") 


    display formula @?= "¬(q ∧ r)"

multipleSameConnective :: TestTree
multipleSameConnective = testCase "multiple connective of same nature" $ do
    let formula = 
            Op @Or
                (MkF $ Atom "p") $
                MkF $ Op @Or
                    (MkF $ Atom "q") 
                    (MkF $ Atom "r") 

    display formula @?= "p ∨ q ∨ r"



multipleConnectiveDifferentPriority :: TestTree
multipleConnectiveDifferentPriority = testCase "multiple connective with ≠ priority" $ do
    let formula = 
            Op @Or
                (MkF $ Atom "p") $
                MkF $ Op @And
                    (MkF $ Atom "q") 
                    (MkF $ Atom "r") 

    display formula @?= "p ∨ q ∧ r"

    let formula = 
            Op @And
                (MkF $ Atom "p") $
                MkF $ Op @Or
                    (MkF $ Atom "q") 
                    (MkF $ Atom "r") 

    display formula @?= "p ∧ (q ∨ r)"


