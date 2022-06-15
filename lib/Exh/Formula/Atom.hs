module Exh.Formula.Atom where

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


-- | syntactic sugar
atom :: AtomName -> Formula
atom = MkF . (Formula_ []) . Atom 

getAtoms :: Formula -> Set AtomName
getAtoms = foldFormula combine where
    combine x children = case cast x of
        Just (Atom name) -> Set.singleton name
        _ -> Set.unions $! children
