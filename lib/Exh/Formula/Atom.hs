{-|
This module defines atoms: the smallest units from which all formulas are built. An atom is defined by a name (type 'AtomName'). The name is used for display.

If -XOverloadedStrings is on, @atom "someName"@ creates a formula representing an atom with name "someName".
-}
module Exh.Formula.Atom(
    Atom(..)
  , atom
) where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Typeable

import Exh.Formula.Internal

------------------- ATOM -----------------

data Atom = Atom AtomName deriving (Eq, Show)


instance IsFormula Atom where
    display_ (Atom (AtomName nameStr)) _ = (0, nameStr)
    evaluate_ g (Formula_ {..}) =
        let Atom name = userData in 
        maybe 
            (Left $ NoValueFor name) 
            Right $
            Map.lookup name g 
    alts_ _ f = [MkF f]

    getAtoms_ f = Set.singleton $ name
                  where Atom name = userData f

-- | Make a proposition atom from a name.
atom :: AtomName -> Formula
atom = MkF . (Formula_ []) . Atom 

