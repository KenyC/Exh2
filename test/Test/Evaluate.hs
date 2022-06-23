{-# LANGUAGE OverloadedStrings #-}
module Test.Evaluate where

import Data.Default
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map
import qualified Data.Set as Set

import Exh.Formula.Atom
import Exh.Formula.Quantifier
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
                , quantifiers 
                , mutipleConnective ]


simpleAssignment :: Assignment
simpleAssignment = 
    mkG $ do
        "y" =@ 2
        "z" =@ 1
        "p" ~> True
        "q" ~> False
        "a" ~> [True, False, True]

getAtomsTest :: TestTree
getAtomsTest = testCase "function 'getAtoms'" $ do
    let p:q:r:[] = map prop ["p", "q", "r"]

    getAtoms p             @?= Set.fromList (map (Atom 0) ["p"])
    getAtoms (p &. neg q)  @?= Set.fromList (map (Atom 0) ["p", "q"])
    getAtoms (p &. q |. q) @?= Set.fromList (map (Atom 0) ["p", "q"])
    getAtoms (r &. q |. p) @?= Set.fromList (map (Atom 0) ["p", "q", "r"])

    let opts = def {_stipulatedAlts = Just [q]}
    getAtoms (exhWith opts p) @?= Set.fromList [Atom 0 "p", Atom 0 "q"]


simpleAtom :: TestTree
simpleAtom = testCase "atom" $ do
    let formula = Formula_ [] $ Predicate (Atom 0 "p") [] 
    evaluate_ simpleAssignment formula @?= Right True

    let formula = Formula_ [] $ Predicate (Atom 0 "q") [] 
    evaluate_ simpleAssignment formula @?= Right False

    let formula = Formula_ [] $ Predicate (Atom 0 "r") [] 
    evaluate_ simpleAssignment formula @?= Left (NoValueFor "r")

    let formula = prop "p" 
    evaluate simpleAssignment formula @?= Right True

    let formula = prop "q" 
    evaluate simpleAssignment formula @?= Right False

    let formula = prop "r" 
    evaluate simpleAssignment formula @?= Left (NoValueFor "r")

    let formula = Formula_ [] $ Predicate (Atom 1 "a") ["x"] 
    evaluate_ simpleAssignment formula @?= Left (UnvaluedVar "x")

    let formula = Formula_ [] $ Predicate (Atom 1 "a") ["y"] 
    evaluate_ simpleAssignment formula @?= Right True

    let formula = Formula_ [] $ Predicate (Atom 1 "a") ["z"] 
    evaluate_ simpleAssignment formula @?= Right False

simpleConnective :: TestTree
simpleConnective = testCase "simple connective" $ do
    let formula = prop "p" |. prop "q"

    evaluate simpleAssignment formula @?= Right True

    let formula = prop "p" &. prop "q"

    evaluate simpleAssignment formula @?= Right False


    let formula = prop "p" &. prd "a" ["y"]
    evaluate simpleAssignment formula @?= Right True


    let formula = prop "p" &. prd "a" ["z"]
    evaluate simpleAssignment formula @?= Right False


    let formula = prop "p" &. prd "b" ["z"]
    evaluate simpleAssignment formula @?= Left (NoValueFor "b")



quantifiers :: TestTree
quantifiers = testCase "quantifiers" $ do
    let [p, q, r] = map prd ["p", "q", "r"]
    let formula = _A "x" $ p ["x"] |.  q ["x"]

    let g = 
          mkG $ do
            "p" ~> [True, False, True]
            "q" ~> [False, True, True] 

    evaluate g (_A "x" $ p ["x"])            @?= Right False
    evaluate g (_A "x" $ p ["x"] |. q ["x"]) @?= Right True
    evaluate g (_A "x" $ p ["y"] |. q ["x"]) @?= Left (UnvaluedVar "y")

    let g' = modG g $ "y" =@ 0
    evaluate g' (_E "x" $ p ["y"] |. q ["x"]) @?= Right True

    

mutipleConnective :: TestTree
mutipleConnective = testCase "multiple connective" $ do
    let formula = prop "p" |. (prop "q" &. prop "r")
    let assignment = 
          mkG $ do
            "p" ~> True
            "q" ~> False 
            "r" ~> False 


    evaluate assignment formula @?= Right True

    let assignment = 
          mkG $ do
            "p" ~> False
            "q" ~> True 
            "r" ~> False 

    evaluate assignment formula @?= Right False

    -- check that there is no short circuit evaluation
    let assignment = 
          mkG $ do
            "p" ~> True
            "q" ~> True 

    evaluate assignment formula @?= Left (NoValueFor "r")

    let p:q:r:[] = map prop ["p", "q", "r"]
        formula = p |. q &. r
        expected =
            [ False, False, False, True
            , True, True, True, True ]
    evalMulti (fullLogicalSpace [Atom 0 "p", Atom 0 "q", Atom 0 "r"]) formula @?= Right expected


logicalSpace :: TestTree
logicalSpace = testCase "logical space" $ do
    let trivialMap x = Map.fromList [([], x)]
    let expected = 
            map (Assignment Map.empty)
            [ Map.fromList [(Atom 0 "a", trivialMap False), (Atom 0 "b", trivialMap False), (Atom 0 "c", trivialMap False) ]
            , Map.fromList [(Atom 0 "a", trivialMap False), (Atom 0 "b", trivialMap False), (Atom 0 "c", trivialMap True)  ]
            , Map.fromList [(Atom 0 "a", trivialMap False), (Atom 0 "b", trivialMap True),  (Atom 0 "c", trivialMap False) ]
            , Map.fromList [(Atom 0 "a", trivialMap False), (Atom 0 "b", trivialMap True),  (Atom 0 "c", trivialMap True)  ]
            , Map.fromList [(Atom 0 "a", trivialMap True),  (Atom 0 "b", trivialMap False), (Atom 0 "c", trivialMap False) ]
            , Map.fromList [(Atom 0 "a", trivialMap True),  (Atom 0 "b", trivialMap False), (Atom 0 "c", trivialMap True)  ]
            , Map.fromList [(Atom 0 "a", trivialMap True),  (Atom 0 "b", trivialMap True),  (Atom 0 "c", trivialMap False) ]
            , Map.fromList [(Atom 0 "a", trivialMap True),  (Atom 0 "b", trivialMap True),  (Atom 0 "c", trivialMap True)  ] ]

    fullLogicalSpace [Atom 0 "a", Atom 0 "b", Atom 0 "c"] @?= expected

    -- edge case
    fullLogicalSpace [] @?= [Assignment Map.empty Map.empty]

    let expected = 
            map (Assignment Map.empty)
                [ Map.fromList [(Atom 1 "a", Map.fromList $ zip [[0], [1], [2]]  [False, False, False ])] 
                , Map.fromList [(Atom 1 "a", Map.fromList $ zip [[0], [1], [2]]  [False, False, True  ])] 
                , Map.fromList [(Atom 1 "a", Map.fromList $ zip [[0], [1], [2]]  [False, True,  False ])] 
                , Map.fromList [(Atom 1 "a", Map.fromList $ zip [[0], [1], [2]]  [False, True,  True  ])] 
                , Map.fromList [(Atom 1 "a", Map.fromList $ zip [[0], [1], [2]]  [True,  False, False ])] 
                , Map.fromList [(Atom 1 "a", Map.fromList $ zip [[0], [1], [2]]  [True,  False, True  ])] 
                , Map.fromList [(Atom 1 "a", Map.fromList $ zip [[0], [1], [2]]  [True,  True,  False ])] 
                , Map.fromList [(Atom 1 "a", Map.fromList $ zip [[0], [1], [2]]  [True,  True,  True  ])] ]

    fullLogicalSpace [Atom 1 "a"] @?= expected



truthTable :: TestTree
truthTable = testCase "truth table" $ do
    let formula = prop "p" &. prop "q"


    evalMulti (fullLogicalSpace [Atom 0 "p", Atom 0 "q"]) formula @?= Right [False, False, False, True]

