{-# LANGUAGE OverloadedStrings #-}
module Test.Evaluate where

import Data.Default
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map
import qualified Data.Set as Set

import Exh.Formula.Atom
import Exh.Formula.Internal
import Exh.Formula.Neg
import Exh.Formula.Op
import Exh.Exh
import Exh.Semantics

allTests :: TestTree
allTests = testGroup 
                "evaluate"
                [ simpleAtom 
                , getAtomsTest 
                , logicalSpace 
                , truthTable 
                , simpleConnective 
                , mutipleConnective ]


simpleAssignment :: Assignment Bool
simpleAssignment = 
    Map.fromList
        [ ("p", True)
        , ("q", False) ]

getAtomsTest :: TestTree
getAtomsTest = testCase "function 'getAtoms'" $ do
    let p:q:r:[] = map atom ["p", "q", "r"]

    getAtoms p             @?= Set.fromList ["p"]  
    getAtoms (p &. neg q)  @?= Set.fromList ["p", "q"]  
    getAtoms (p &. q |. q) @?= Set.fromList ["p", "q"]  
    getAtoms (r &. q |. p) @?= Set.fromList ["p", "q", "r"]  

    let opts = def {_stipulatedAlts = Just [q]}
    getAtoms (exhWith opts p) @?= Set.fromList ["p", "q"]
simpleAtom :: TestTree
simpleAtom = testCase "atom" $ do
    let formula = Formula_ [] $ Atom "p" 
    evaluate_ simpleAssignment formula @?= Right True

    let formula = Formula_ [] $ Atom "q" 
    evaluate_ simpleAssignment formula @?= Right False

    let formula = Formula_ [] $ Atom "r" 
    evaluate_ simpleAssignment formula @?= Left (NoValueFor "r")

    let formula = atom "p" 
    evaluate simpleAssignment formula @?= Right True

    let formula = atom "q" 
    evaluate simpleAssignment formula @?= Right False

    let formula = atom "r" 
    evaluate simpleAssignment formula @?= Left (NoValueFor "r")

simpleConnective :: TestTree
simpleConnective = testCase "simple connective" $ do
    let formula = 
            Formula_ 
                [ MkF $ Formula_ [] $ Atom "p"  
                , MkF $ Formula_ [] $ Atom "q" ] 
                (Op @Or)

    evaluate_ simpleAssignment formula @?= Right True

    let formula = 
            Formula_ 
                [ MkF $ Formula_ [] $ Atom "p" 
                , MkF $ Formula_ [] $ Atom "q" ]
                (Op @And)

    evaluate_ simpleAssignment formula @?= Right False

    let formula = (atom "p") &. (atom "q")
    evaluate simpleAssignment formula @?= Right False


mutipleConnective :: TestTree
mutipleConnective = testCase "multiple connective" $ do
    let formula = atom "p" |. (atom "q" &. atom "r")
    let assignment = 
          Map.fromList
            [ ("p", True)
            , ("q", False) 
            , ("r", False) ]


    evaluate assignment formula @?= Right True

    let assignment = 
          Map.fromList
            [ ("p", False)
            , ("q", True) 
            , ("r", False) ]

    evaluate assignment formula @?= Right False

    -- check that there is no short circuit evaluation
    let assignment = 
          Map.fromList
            [ ("p", True)
            , ("q", True) ]

    evaluate assignment formula @?= Left (NoValueFor "r")

    let p:q:r:[] = map atom ["p", "q", "r"]
        formula = p |. q &. r
        expected =
            [ False, False, False, True
            , True, True, True, True ]
    evalMulti (fullLogicalSpace ["p", "q", "r"]) formula @?= Right expected


logicalSpace :: TestTree
logicalSpace = testCase "logical space" $ do
    let expected = 
            [ Map.fromList [("a", False), ("b", False), ("c", False) ]
            , Map.fromList [("a", False), ("b", False), ("c", True)  ]
            , Map.fromList [("a", False), ("b", True),  ("c", False) ]
            , Map.fromList [("a", False), ("b", True),  ("c", True)  ]
            , Map.fromList [("a", True),  ("b", False), ("c", False) ]
            , Map.fromList [("a", True),  ("b", False), ("c", True)  ]
            , Map.fromList [("a", True),  ("b", True),  ("c", False) ]
            , Map.fromList [("a", True),  ("b", True),  ("c", True)  ] ]

    fullLogicalSpace ["a", "b", "c"] @?= expected

    -- edge case
    fullLogicalSpace [] @?= [Map.fromList []]



truthTable :: TestTree
truthTable = testCase "truth table" $ do
    let formula = 
            MkF $ Formula_  
                [ MkF $ Formula_ [] $ Atom "p" 
                , MkF $ Formula_ [] $ Atom "q" ]
                (Op @And)


    evalMulti (fullLogicalSpace ["p", "q"]) formula @?= Right [False, False, False, True]

