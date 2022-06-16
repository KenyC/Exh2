{-# LANGUAGE OverloadedStrings #-}
module Test.Exh where

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
                "exh"
                [ simpleAtomExhaustify 
                , simpleConnectiveExhaustify 
                , multipleConnectiveExhaustify 
                , simpleConnectiveExh 
                , multipleConnectiveExh 
                , orToAnd 
                , pOrQOrR ]


simpleAtomExhaustify :: TestTree
simpleAtomExhaustify = testCase "atom" $ do
    let Just formula = getData @Exh $ exh $ atom "p" 
    allAlts formula  @?= [atom "p"]
    ieAlts  formula  @?= []


simpleConnectiveExhaustify :: TestTree
simpleConnectiveExhaustify = testCase "one connective" $ do
    let p = atom "p"
    let q = atom "q"

    sameElems
        (ieExhaustify (p &. q) [p, q, p |. q])
        []

    sameElems
        (ieExhaustify (p |. q) [p, q, p &. q])
        [p &. q]

    sameElems
        (ieExhaustify (p |. q) [p, q])
        []


multipleConnectiveExhaustify :: TestTree
multipleConnectiveExhaustify = testCase "multiple connective" $ do

    let p = atom "p"
    let q = atom "q"
    let r = atom "r"

    sameElems
        (ieExhaustify (p |. (q |. r)) [p, q, r, p |. q, p |. r, q |. r])
        []

    sameElems
        (ieExhaustify (p |. (q |. r)) [p, q, r, p &. q, p &. r, q &. r])
        [p &. q, p &. r, q &. r]






exhOpts :: ExhOptions
exhOpts = ExhOptions {
    _scaleGen = ScaleGen {
          _opScales = [Or <|> And]
        , _subst    = True 
    }
    , _stipulatedAlts = Nothing
}





simpleConnectiveExh :: TestTree
simpleConnectiveExh = testCase "one connective" $ do
    let p = atom "p"
    let q = atom "q"
    let r = atom "r"


    let formula = exhWith exhOpts $ p |. (q |. r)
    let Just dataExh = getData @Exh $ formula

    sameElems
        (ieExhaustify (p &. q) [p, q, p |. q])
        []

    sameElems
        (ieExhaustify (p |. q) [p, q, p &. q])
        [p &. q]

    sameElems
        (ieExhaustify (p |. q) [p, q])
        []


multipleConnectiveExh :: TestTree
multipleConnectiveExh = testCase "multiple connective" $ do
    let p = atom "p"
    let q = atom "q"
    let r = atom "r"

    let formula = exhWith exhOpts $ p |. (q |. r)
    let Just dataExh = getData @Exh $ formula

    sameElems
        (allAlts dataExh)
        [ p |. (q |. r)
        , p |. (q &. r)
        , p &. (q |. r)
        , p &. (q &. r) 
        , (q &. r) 
        , (q |. r) 
        , (p &. q) 
        , (p |. q) 
        , (p &. r) 
        , (p |. r) 
        , p, q, r ]

    sameElems
        (ieAlts dataExh)
        [ p &. (q |. r)
        , p &. (q &. r)
        , (q &. r)
        , (p &. r)
        , (p &. q) ]

    let formula = exhWith exhOpts $ p |. q
    let expectedEq = (p |. q) &. neg (p &. q)
    equivalent formula expectedEq @?= Right True 

    let formula = exhWith exhOpts $ p |. q |. r
    let expectedEq = (p |. q |. r) &. neg (p &. q) &. neg (p &. r) &. neg (r &. q)
    equivalent formula expectedEq @?= Right True 

pOrQOrR :: TestTree
pOrQOrR = testCase "'p or q or r' has an exactly 1 reading " $ do
    let p = atom "p"
    let q = atom "q"
    let r = atom "r"

    let f = exh $ p |. q |. r
        expected = (p &. neg q &. neg r) |. (neg p &. neg q &. r) |. (neg p &. q &. neg r)

    let altsIE = maybe [] ieAlts $ getData @Exh f

    print $ mkTruthTable $ f:altsIE

    equivalent f expected @?= Right True

orToAnd :: TestTree
orToAnd = testCase "'or' strengthens to 'and' when no 'and' competitor" $ do
    let p = atom "p"
    let q = atom "q"

    let exhOpts :: ExhOptions
        exhOpts = ExhOptions {
            _scaleGen = ScaleGen {
                  _opScales = []
                , _subst    = True 
            }
            , _stipulatedAlts = Nothing
        }

    let innerFormula = exhWith exhOpts $ p |. q
    let Just innerData = getData @Exh innerFormula
    let formula = exhWith exhOpts $ innerFormula
    let Just allData = getData @Exh formula


    allAlts innerData `sameElems` [p |. q, p, q]
    ieAlts innerData  `sameElems` []


    -- length (allAlts allData) @?= 3 
    -- ieAlts allData  `sameElems` [p &. neg q, q &. neg p]

    putStrLn $ displayTruthTable $ mkTruthTable $ allAlts allData

    equivalent formula (p &. q) @?= Right True
