{-# LANGUAGE OverloadedStrings #-}
module Test.Alts where

import Data.Default
import Test.Tasty
import Test.Tasty.HUnit

import Utils

import Exh.Internal
import Exh.Atom
import Exh.Quantifier
import Exh.Proposition
import Exh.Exh

allTests :: TestTree
allTests = testGroup 
                "alts"
                [ simpleAtom  
                , simpleConnective  
                , quantifiers      ]


simpleAtom :: TestTree
simpleAtom = testCase "atom" $ do
    let formula = prop "xyz" 
    alts def formula @?= [formula]

    let formula = prd "xyz" ["x"] 
    alts def formula @?= [formula]

    let formula = prd "xyz" ["x", "z"] 
    alts def formula @?= [formula]

simpleConnective :: TestTree
simpleConnective = testCase "simple connective" $ do
    let p:q:[] = map prop ["p", "q"]
    let formula = p |. q 

    let opts = def { _subst = False }

    sameElems
        (alts opts formula)  
        [p |. q, p &. q]

    let formula = p &. q 
    sameElems 
        (alts opts formula) 
        [p &. q]

    sameElems
        [p &. q, p, q]
        (alts def formula)  

    -- let formula = p &. q 
    -- alts_ def formula @?= [p &. q, p, q]


quantifiers :: TestTree
quantifiers = testCase "quantifiers" $ do
    let p:q:[] = map prd ["p", "q"]
    let formula = _E "x" $ p ["x"] 

    let opts = def 

    sameElems
        (alts opts formula)  
        [_E "x" $ p ["x"], _A "x" $ p ["x"]]


    let formula = _E "x" $ p ["x"] |. q ["x"] 

    sameElems
        (alts opts formula)  
        [ _E "x" $ p ["x"], _E "x" $ q ["x"]
        , _E "x" $ p["x"] |. q ["x"], _E "x" $ p["x"] &. q ["x"] 
        , _A "x" $ p ["x"], _A "x" $ q ["x"]
        , _A "x" $ p["x"] |. q ["x"], _A "x" $ p["x"] &. q ["x"] ]


    -- let formula = p &. q 
    -- alts_ def formula @?= [p &. q, p, q]


-- negation :: TestTree
-- negation = testCase "negation" $ do
--     let formula = 
--             Neg (MkF $ Atom "p") 

--     display formula @?= "¬p"

--     let formula = 
--             Neg $  
--             MkF $ Op orConnective
--                 (MkF $ Atom "q") 
--                 (MkF $ Atom "r") 


--     display formula @?= "¬(q ∨ r)"

--     let formula = 
--             Neg $  
--             MkF $ Op andConnective
--                 (MkF $ Atom "q") 
--                 (MkF $ Atom "r") 


--     display formula @?= "¬(q ∧ r)"

-- multipleSameConnective :: TestTree
-- multipleSameConnective = testCase "multiple connective of same nature" $ do
--     let formula = 
--             Op orConnective
--                 (MkF $ Atom "p") $
--                 MkF $ Op orConnective
--                     (MkF $ Atom "q") 
--                     (MkF $ Atom "r") 

--     display formula @?= "p ∨ q ∨ r"



-- multipleConnectiveDifferentPriority :: TestTree
-- multipleConnectiveDifferentPriority = testCase "multiple connective with ≠ priority" $ do
--     let formula = 
--             Op orConnective
--                 (MkF $ Atom "p") $
--                 MkF $ Op andConnective
--                     (MkF $ Atom "q") 
--                     (MkF $ Atom "r") 

--     display formula @?= "p ∨ q ∧ r"

--     let formula = 
--             Op andConnective
--                 (MkF $ Atom "p") $
--                 MkF $ Op orConnective
--                     (MkF $ Atom "q") 
--                     (MkF $ Atom "r") 

--     display formula @?= "p ∧ (q ∨ r)"


