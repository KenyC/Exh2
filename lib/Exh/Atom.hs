{-|
This module defines atoms: the smallest units from which all formulas are built. An atom is defined by a name (type 'AtomName'). The name is used for display.

If -XOverloadedStrings is on, @atom "someName"@ creates a formula representing an atom with name "someName".
-}
module Exh.Atom(
    Atom(..)
  , Predicate(..)
  , sizeDomain
  , prop
  , prd
) where


import Control.Monad
import Data.List (intersperse)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Exh.Internal

------------------- ATOM -----------------


data Predicate = Predicate {
    atom        :: Atom
  , varsInSlots :: [VarName]
} deriving (Eq, Show)

instance IsFormula Predicate where
    display_ (Predicate Atom{..} vars) _ = (0, displayString) where
        AtomName nameStr = name
        displayString
            | arity == 0 = nameStr 
            | otherwise  = nameStr ++ "(" ++ (mconcat $ intersperse "," [varStr | VarName varStr <- vars])  ++ ")"


    evaluate_ Assignment{..} f = do
        let maybeToEither _  (Just x)  = Right x
            maybeToEither err Nothing  = Left err
        let Predicate {..} = userData f
        valueVars <- forM varsInSlots $ \var ->
                        maybeToEither (UnvaluedVar var) $ Map.lookup var varVals
        atomAssignment <- maybeToEither (NoValueFor $ name atom) $ Map.lookup atom atomVals 
        maybeToEither _somethingElse $ Map.lookup valueVars atomAssignment


    alts_ _ f = [MkF f]

    getAtoms_ f = Set.singleton $ atom $ userData f
    freeVars_ f = Set.fromList  $ varsInSlots $ userData f

-- | Make a proposition atom from a name.
prop :: AtomName -> Formula
prop = MkF . (Formula_ []) . (\name -> Predicate (Atom 0 name) []) 

prd :: AtomName -> [VarName] -> Formula
prd name vars = MkF $ Formula_ [] $ Predicate (Atom (length vars) name) vars 

sizeDomain :: Int
sizeDomain = 3

