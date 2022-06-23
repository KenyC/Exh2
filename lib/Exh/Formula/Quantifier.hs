module Exh.Formula.Quantifier(
    ForAll(..),
    Exists(..),
    QuantifierDesc(..),
    Quantifier(..),
    _A, _E,
    (<::>),
) where

import Exh.Formula.Internal
import Exh.Formula.Atom
import Data.Typeable
import Data.Proxy
import Data.Maybe (catMaybes)
import Data.List  (nub)


data QuantifierDesc = QuantifierDesc {
    symbol :: String         -- ^ used for display: is symbol is "@", then formulas are displayed as "@x, p(x)"
  , fun    :: [Bool] -> Bool -- ^ how to combine truth-values from each quantificational case into the truth-value of the whole
}

data Quantifier qType = Quantifier VarName
-- TODO: not correct, does not take into account renaming (∀x, p(x) == ∀y, p(y))
deriving instance (IsQuantifier q) => Eq (Quantifier q) 

class (Typeable q) => IsQuantifier q where 
    quant :: Proxy q -> QuantifierDesc

data ForAll = ForAll
instance IsQuantifier ForAll where
    quant _ = QuantifierDesc {
        symbol = "∀"
      , fun    = all id
    }

data Exists = Exists
instance IsQuantifier Exists where
    quant _ = QuantifierDesc {
        symbol = "∃"
      , fun    = any id
    }

instance (IsQuantifier q) => IsFormula (Quantifier q) where
    display_ (Quantifier (VarName var)) [(_, scopeStr)] = 
        let quantSymbol = symbol $ quant $ Proxy @q in
        (4, quantSymbol ++ var ++ ", " ++ scopeStr)
    display_ _ _ = error "Quantifiers may only have one child!"

    evaluate_ g (Formula_ [scope] (Quantifier name)) = do 
        let funCombine = fun $ quant (Proxy @q)
            assignments = 
                [ modG g $ name =@ i
                | i <- [0 .. sizeDomain - 1] ]
        results <- evalMulti assignments scope
        return $! funCombine results
    evaluate_ _ _ = error "Quantifiers shoud have exactly one child" 


    alts_ sg@ScaleGen{..} f@Formula_{..} = let
        [p] = children
        !altsP = alts sg p
        altsSameOp = 
            [ f {children = [altP]}
            | altP <- altsP ]

        scalarAlts = catMaybes
            [ applyScale altSameRoot scale
            | altSameRoot <- altsSameOp
            , scale       <- _opScales     ]
        in nub $ (map MkF altsSameOp) ++ scalarAlts


_A :: VarName -> Formula -> Formula
_A name scope = MkF $ Formula_ [scope] $ Quantifier @ForAll name

_E :: VarName -> Formula -> Formula
_E name scope = MkF $ Formula_ [scope] $ Quantifier @Exists name


------------------- SCALES -----------------

instance (Typeable op1, Typeable op2) => MapUserData (Quantifier op1) (Quantifier op2) where
    mapUserData (Quantifier var) = Quantifier var

(<::>) :: forall q1 q2. (IsQuantifier q1, IsQuantifier q2) => q1 -> q2 -> Scale
(<::>) _ _ = Scale (Proxy @(Quantifier q1)) (Proxy @(Quantifier q2))