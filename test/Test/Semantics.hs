{-# LANGUAGE OverloadedStrings #-}
module Test.Semantics where

import Test.Tasty
import Test.Tasty.HUnit

import Utils

import Exh.Formula.Atom
import Exh.Formula.Internal
import Exh.Formula.Neg
import Exh.Formula.Op
import Exh.Exh
import Exh.Semantics

allTests :: TestTree
allTests = testGroup 
                "semantics"
                [ deMorgan 
                , strongerConnectives ]


p = atom "p"
q = atom "q"
r = atom "r"

deMorgan :: TestTree
deMorgan = testCase "De Morgan" $ do

    let notAnd = neg $ p &. q
    let orNot  = neg p |. neg q
    let andNot = neg p &. neg q
    let notOr  = neg $ p |. q

    equivalent notOr  andNot @?= Right True
    equivalent notAnd orNot  @?= Right True
    equivalent notAnd andNot @?= Right False
    equivalent notOr  orNot  @?= Right False


strongerConnectives :: TestTree
strongerConnectives = testCase "strongerConnectives" $ do
    entails (p &. q) (p |. q) @?= Right True
    entails (p |. q) (p &. q) @?= Right False


    let f = (p |. q) &. (q |. r) &. (p |. r)
        g = (p &. q) |. (q &. r) |. (p &. r)

    entails f g @?= Right True
    entails g f @?= Right True

    let f = p |. neg q
        g = neg p |. q
        
    compatible f g             @?= Right True
    compatible (neg f) (neg g) @?= Right False
        