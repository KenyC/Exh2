{-# LANGUAGE OverloadedStrings #-}
module Test.Evaluate where


import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map

import Formula

allTests :: TestTree
allTests = testGroup 
                "evaluate"
                [ simpleAtom 
                , logicalSpace 
                , truthTable 
                , simpleConnective 
                , mutipleConnective ]

simpleAssignment :: Assignment Bool
simpleAssignment = 
    Map.fromList
        [ ("p", True)
        , ("q", False) ]


simpleAtom :: TestTree
simpleAtom = testCase "atom" $ do
    let formula = Atom "p" 
    evaluate_ simpleAssignment formula @?= Right True

    let formula = Atom "q" 
    evaluate_ simpleAssignment formula @?= Right False

    let formula = Atom "r" 
    evaluate_ simpleAssignment formula @?= Left (NoValueFor "r")

    let formula = atom "p" 
    evaluate_ simpleAssignment formula @?= Right True

    let formula = atom "q" 
    evaluate_ simpleAssignment formula @?= Right False

    let formula = atom "r" 
    evaluate_ simpleAssignment formula @?= Left (NoValueFor "r")

simpleConnective :: TestTree
simpleConnective = testCase "simple connective" $ do
    let formula = 
            Op orConnective
                (MkF $ Atom "p") 
                (MkF $ Atom "q") 

    evaluate_ simpleAssignment formula @?= Right True

    let formula = 
            Op andConnective
                (MkF $ Atom "p") 
                (MkF $ Atom "q") 

    evaluate_ simpleAssignment formula @?= Right False

    let formula = (atom "p") .& (atom "q")
    evaluate_ simpleAssignment formula @?= Right False


mutipleConnective :: TestTree
mutipleConnective = testCase "multiple connective" $ do
    let formula = 
            Op orConnective
                (MkF $ Atom "p") $
                MkF $ Op andConnective
                    (MkF $ Atom "q") 
                    (MkF $ Atom "r") 

    let assignment = 
          Map.fromList
            [ ("p", True)
            , ("q", False) 
            , ("r", False) ]


    evaluate_ assignment formula @?= Right True

    let assignment = 
          Map.fromList
            [ ("p", False)
            , ("q", True) 
            , ("r", False) ]

    evaluate_ assignment formula @?= Right False

    -- check that there is no short circuit evaluation
    let assignment = 
          Map.fromList
            [ ("p", True)
            , ("q", True) ]

    evaluate_ assignment formula @?= Left (NoValueFor "r")

    let p:q:r:[] = map atom ["p", "q", "r"]
        formula = p .| q .& r
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
            MkF $ Op andConnective
                (MkF $ Atom "p") 
                (MkF $ Atom "q") 


    evalMulti (fullLogicalSpace ["p", "q"]) formula @?= Right [False, False, False, True]

