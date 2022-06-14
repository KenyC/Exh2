{-# LANGUAGE OverloadedStrings #-}
module Test.Exh where

import Test.Tasty
import Test.Tasty.HUnit

import Utils

import Exh.Formula
import Exh.Exh

allTests :: TestTree
allTests = testGroup 
                "display"
                [ simpleAtom 
                , simpleConnective ]


simpleAtom :: TestTree
simpleAtom = testCase "atom" $ do
    let Just formula = matching @Exh $ exh $ atom "p" 
    alts formula   @?= [atom "p"]
    ieAlts formula @?= []


simpleConnective :: TestTree
simpleConnective = testCase "connective" $ do
    let p = atom "p"
    let q = atom "q"

    sameElems
        (ieExhaustify (p .& q) [p, q, p .| q])
        []

    sameElems
        (ieExhaustify (p .| q) [p, q, p .& q])
        [p .& q]

    sameElems
        (ieExhaustify (p .| q) [p, q])
        []
