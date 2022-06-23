{-# LANGUAGE OverloadedStrings #-}
module Test.Exh where

import Test.Tasty
import Test.Tasty.HUnit

import Utils

import Exh.Formula.Atom
import Exh.Formula.Quantifier
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
                , someButNotAllExh 
                , freeChoice 
                , distributiveImplicature
                , orToAnd 
                , pOrQOrR ]


simpleAtomExhaustify :: TestTree
simpleAtomExhaustify = testCase "atom" $ do
    let Just formula = getData @Exh $ exh $ prop "p" 
    allAlts formula  @?= [prop "p"]
    ieAlts  formula  @?= []


simpleConnectiveExhaustify :: TestTree
simpleConnectiveExhaustify = testCase "one connective" $ do
    let p = prop "p"
    let q = prop "q"

    sameElems
        (ieExhaustify (p &. q) [p, q, p |. q])
        []

    sameElems
        (ieExhaustify (p |. q) [p, q, p &. q])
        [p &. q]

    sameElems
        (ieExhaustify (p |. q) [p, q])
        []

    let p = prd "p"
    let q = prd "q"

    sameElems
        (ieExhaustify (p ["x"] |. q ["x"]) [p ["x"] |. q ["x"], p ["x"], q ["x"], p ["x"] &. q ["x"]])
        [p ["x"] &. q ["x"]]




multipleConnectiveExhaustify :: TestTree
multipleConnectiveExhaustify = testCase "multiple connective" $ do

    let p = prop "p"
    let q = prop "q"
    let r = prop "r"

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
    let p = prop "p"
    let q = prop "q"
    let r = prop "r"


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
    let p = prop "p"
    let q = prop "q"
    let r = prop "r"

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
    let p = prop "p"
    let q = prop "q"
    let r = prop "r"

    let f = exh $ p |. q |. r
        expected = (p &. neg q &. neg r) |. (neg p &. neg q &. r) |. (neg p &. q &. neg r)


    equivalent f expected @?= Right True

someButNotAllExh :: TestTree
someButNotAllExh = testCase "'some but not all' implicatures" $ do
    let p = prd "p"

    let f = exh $ _E "x" $ p ["x"]
        expected = (_E "x" $ p ["x"]) &. neg (_A "x" $ p ["x"])

    equivalent f expected           @?= Right True
    equivalent f (_E "x" $ p ["x"]) @?= Right False


    ------------------- INDIRECT IMPLICATURES -----------------
    let exhOpts :: ExhOptions
        exhOpts = ExhOptions {
            _scaleGen = ScaleGen {
                  _opScales = [Exists <::> ForAll, ForAll <::> Exists]
                , _subst    = True 
            }
            , _stipulatedAlts = Nothing
        }



    let f = exhWith exhOpts $ _A "x" $ p ["x"]
        expected = _A "x" $ p ["x"]

    equivalent f expected @?= Right True
    
    let f = exhWith exhOpts $ neg $ _A "x" $ p ["x"]
        expected = (_E "x" $ p ["x"]) &. neg (_A "x" $ p ["x"])

    -- let altsIE = maybe [] ieAlts $ getData @Exh f
    -- let altsAll = maybe [] allAlts $ getData @Exh f
    -- print $ mkTruthTable $ f:altsAll

    equivalent f expected @?= Right True



freeChoice :: TestTree
freeChoice = testCase "Free Choice implicatures" $ do
    let p = prop "p"
        q = prop "q"

    -- without "and" alternative, "or" strengthens to and
    let exhOpts :: ExhOptions
        exhOpts = ExhOptions {
            _scaleGen = ScaleGen {
                  _opScales = []
                , _subst    = True 
            }
            , _stipulatedAlts = Nothing
        }

    let exh' = exhWith exhOpts

    let prejacent = exh' $ p |. q 
    let f = exh' prejacent
        expected = p &. q



    -- let altsAll = maybe [] allAlts $ getData @Exh f
    -- print $ mkTruthTable $ f:altsAll

    equivalent f expected @?= Right True

    -- fine-grained testing
    let altsIE = maybe undefined ieAlts $ getData @Exh prejacent
    altsIE @?= []

    -- it's difficult to construct the right formula ; we just check that this is indeed the right type of exh
    let altsIE = maybe undefined ieAlts $ getData @Exh f
    (map show altsIE) `sameElems` ["Exh(p)", "Exh(q)"]

    -- Now with default options ; free choice under an existential
    let p = prd "p"
        q = prd "q"

    let prejacent = exh $ _E "x" $ p ["x"] |. q ["x"]
        f = exh prejacent

    print $ mkTruthTable $ [f, _E "x" $ p ["x"] &. q ["x"]]

    entails f (_E "x" $ p ["x"]) @?= Right True
    entails f (_E "x" $ q ["x"]) @?= Right True
    compatible f (_E "x" $ p ["x"] &. q ["x"]) @?= Right False



distributiveImplicature :: TestTree
distributiveImplicature = testCase "Distributive implicatures" $ do
    let p = prd "p"
        q = prd "q"

    -- without "Exists" alternative
    let exhOpts :: ExhOptions
        exhOpts = ExhOptions {
            _scaleGen = ScaleGen {
                  _opScales = [Or <|> And]
                , _subst    = True 
            }
            , _stipulatedAlts = Nothing
        }
    let exh' = exhWith exhOpts


    let f = exh' $ _A "x" $ p ["x"] |. q ["x"] 

    entails f (_E "x" $ p ["x"]) @?= Right True
    entails f (_E "x" $ q ["x"]) @?= Right True
    -- too strong an inference, as Crnic et al. discuss
    compatible f (_A "x" $ q ["x"]) @?= Right False

    -- with "Exists" alternative, the SI is embedded
    let exhOpts :: ExhOptions
        exhOpts = ExhOptions {
            _scaleGen = ScaleGen {
                  _opScales = [Or <|> And, ForAll <::> Exists]
                , _subst    = True 
            }
            , _stipulatedAlts = Nothing
        }
    let exh' = exhWith exhOpts
    let f = exh' $ _A "x" $ p ["x"] |. q ["x"] 

    -- no distributive implicature
    entails f (_E "x" $ p ["x"]) @?= Right False
    entails f (_E "x" $ q ["x"]) @?= Right False
    -- but an embedded SI!
    entails f (neg $ _E "x" $ p ["x"] &. q ["x"]) @?= Right True

    -- we can "correct", this state of affairs by 
    -- (1) removing the "and" alternative, 
    -- (2) recursively exhaustifying

    let exhOpts :: ExhOptions
        exhOpts = ExhOptions {
            _scaleGen = ScaleGen {
                  _opScales = [ForAll <::> Exists]
                , _subst    = True 
            }
            , _stipulatedAlts = Nothing
        }
    let exh' = exhWith exhOpts
    let f = exh' $ exh' $ _A "x" $ p ["x"] |. q ["x"] 

    -- no distributive implicature
    entails f (_E "x" $ p ["x"]) @?= Right True
    entails f (_E "x" $ q ["x"]) @?= Right True
    -- no embedded SI
    entails f (neg $ _E "x" $ p ["x"] &. q ["x"]) @?= Right False
    -- no strong distributivity
    compatible f (_A "x" $ p ["x"]) @?= Right True




orToAnd :: TestTree
orToAnd = testCase "'or' strengthens to 'and' when no 'and' competitor" $ do
    let p = prop "p"
    let q = prop "q"

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
