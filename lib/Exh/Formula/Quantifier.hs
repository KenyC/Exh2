{-|
This module contains everything to define quantifiers and quantified 'Formula'. It defines the existential quantifier and the universal quantifier (cf '_A' and '_E').

= Defining quantified statement

'_A' and '_E' take a 'VarName' as an argument, representing the name of the variable bound by the quantifier. That same 'VarName' can then be given as an argument to 'prd'. 

For instance, the formula '∀x, ∃y, p(x, y)' can we written as:

@
p = prd "p"
f = _A "x" $ _E "y" $ p ["x", "y"]
@

= Defining a custom quantifier

To define a custom quantifier, start by defining a type. For instence, the type 'MyQuantifier':

@data MyQuantifier = MyQuantifier@

This type is really just a marker so it does not matter how it is defined (although having at least one value for this type will make it easier to use it in scales). 
Then, make this type an instance of 'IsQuantifier' ; this requires providing a 'QuantifierDesc' value, with two fields: 

   - 'symbol' which is used for display.
   - 'fun' which is used for evaluation: it is a function that turns a list of booleans (the scope evaluated for each value of the variable) in a boolean. 

@
instance IsQuantifier MyQuantifier where
    -- the argument is there for type reason, it contains no information whatsoever
    quant _ = QuantifierDesc {
         symbol = "MyQ"
         -- defining an "exactly 2" interpretation
       , fun = (== 2) . length . filter id 
    }
@

Then the formula "MyQ x, p(x)" may be defined as below (for explanation on how this comes together, cf 'Exh.Formula.Internal')

@
MkF $ Formula_ [p ["x"]] (Quantifier @MyQ "x")
@

That's very cumbersome, as it exposes internal gears we don't want to see. We can camouflage this by defining the following auxiliary:

@
myQ :: VarName -> Formula -> Formula
myQ var scope = MkF $ Formula_ [scope] $ Quantifier @MyQ var
@

With this, "MyQ x, p(x)" becomes more legible:

@
myQ "x" $ p ["x"]
@

= Creating quantifier scales

You can create quantifier scales with the operator '(<::>)'. The operator takes two argument values, whose type is the quantifier type of the scale.

@
someAll :: Scale
someAll = Exists <::> Forall
@

Here, 'Exists' is a value of type 'Exists' and 'ForAll' is a value of type 'ForAll' (following the confusing and convenient convention of naming constructors after their type). With the custom quantifier defined earlier, we could for instance write the following scale:

@
someMyQuantifier :: Scale
someMyQuantifier = Exists <::> MyQuantifier
@

Armed with a custom scale, we can write some 'ExhOptions' for exhaustification with non-default scales:

@
opts :: ExhOptions
opts = ExhOptions {
    _scaleGen = ScaleGen {
          _opScales = [Or <|> And,  Exists <::> MyQuantifier]
        , _subst    = True 
    }
    , _stipulatedAlts = Nothing
}
@

And perform exhaustification with these new scales:

@
exhWith opts $ _E "x" $ prd "p" ["x"]
@

-}
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

-- | Contains all information needed to define a quantifier. 
--   Used in the definition of 'IsQuantifier'.
data QuantifierDesc = QuantifierDesc {
    symbol :: String         -- ^ used for display: is symbol is "@", then formulas are displayed as "@x, p(x)"
  , fun    :: [Bool] -> Bool -- ^ how to combine truth-values from each quantificational case into the truth-value of the whole
}

-- | A quantifier only contains the name of the variable that it binds. 
--   `qType` is simply a marker that tells us (at the type level) which quantifier this is.
--   So an existential is type @Quantifier Exists@, a universal is type @Quantifier ForAll@
--   `qType` ought to be an instance of 'IsQuantifier' ; this means that it is associated to a description (type 'QuantifierDesc') of how to display and evaluate it. 
data Quantifier qType = Quantifier VarName
-- TODO: not correct, does not take into account renaming (∀x, p(x) == ∀y, p(y))
deriving instance (IsQuantifier q) => Eq (Quantifier q) 

-- | An 'IsQuantifier' type is simply a type that has a decription attached to it.
class (Typeable q) => IsQuantifier q where 
    quant :: Proxy q -> QuantifierDesc

-- | Type marker for universal quantifiers.
data ForAll = ForAll
instance IsQuantifier ForAll where
    quant _ = QuantifierDesc {
        symbol = "∀"
      , fun    = all id
    }

-- | Type marker for existential quantifiers.
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


-- | Helper to create a universal statement.  
--   @f = _A "x" $ p ["x"]@
_A :: VarName -> Formula -> Formula
_A name scope = MkF $ Formula_ [scope] $ Quantifier @ForAll name

-- | Helper to create an existential statement.  
--   @f =  _E "y" $ q ["y"]@
_E :: VarName -> Formula -> Formula
_E name scope = MkF $ Formula_ [scope] $ Quantifier @Exists name


------------------- SCALES -----------------

instance (Typeable op1, Typeable op2) => MapUserData (Quantifier op1) (Quantifier op2) where
    mapUserData (Quantifier var) = Quantifier var

-- | Given two values for quantifier types /q1/ and /q2/, creates a scale with /q1/ the weaker element and /q2/ the stronger element.
(<::>) :: forall q1 q2. (IsQuantifier q1, IsQuantifier q2) => q1 -> q2 -> Scale
(<::>) _ _ = Scale (Proxy @(Quantifier q1)) (Proxy @(Quantifier q2))