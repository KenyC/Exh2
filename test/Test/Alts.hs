{-# LANGUAGE OverloadedStrings #-}
module Test.Alts where

import Data.Default
import Test.Tasty
import Test.Tasty.HUnit

import Utils

import Exh.Formula.Internal
import Exh.Formula.Atom
import Exh.Formula.Op
import Exh.Exh

allTests :: TestTree
allTests = testGroup 
                "display"
                [ simpleAtom  
                , simpleConnective  ]


simpleAtom :: TestTree
simpleAtom = testCase "atom" $ do
    let formula = Formula_ [] $ Atom "xyz" 
    alts_ def formula @?= [MkF formula]

simpleConnective :: TestTree
simpleConnective = testCase "simple connective" $ do
    let p:q:[] = map atom ["p", "q"]
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


