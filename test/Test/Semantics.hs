{-# LANGUAGE OverloadedStrings #-}
module Test.Semantics where

import Test.Tasty
import Test.Tasty.HUnit

import Utils

import Exh.Atom
import Exh.Quantifier
import Exh.Internal
import Exh.Proposition
import Exh.Exh
import Exh.Semantics

allTests :: TestTree
allTests = testGroup 
                "semantics"
                [ deMorgan 
                , strongerConnectives 
                , strongerQuantifiers ]


p = prop "p"
q = prop "q"
r = prop "r"

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

    let f = p |. q
        g = p &. q

    entails f g @?= Right False
    entails g f @?= Right True

    let f = p |. neg q
        g = neg p |. q
        
    compatible f g             @?= Right True
    compatible (neg f) (neg g) @?= Right False
        

strongerQuantifiers :: TestTree
strongerQuantifiers = testCase "stronger quantifiers" $ do
    let p = prd "p"
        q = prd "q"

    entails (_A "x" $ p ["x"]) (_E "x" $ p ["x"]) @?= Right True
    entails (_E "x" $ p ["x"]) (_A "x" $ p ["x"]) @?= Right False

    let p = prd "p"
        q = prd "q"

    let f = _A "x" $ p ["x"] |. q ["x"]
        g = (_E "x" $ p ["x"]) |. (_E "x" $ q ["x"]) 

    entails f g @?= Right True
    entails g f @?= Right False
