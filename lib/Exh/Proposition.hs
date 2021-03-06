{-|
This module defines a general interface to create binary operators, as well as useful particular instances like 'Or' and 'And'.

= Writing propositional logic

The operator '(&.)' and '(|.)' are defined for your convenience:

@
p = prop "p"
q = prop "q"
r = prop "r"
f = p |. (q &. r)
@

= Defining a custom operator

To define a custom operator, start by defining a type. For instance, the type 'MyOp':

@data MyOp = MyOp@

This type is really just a marker so it does not matter how it is defined (although having at least one value for this type will make it easier to use it in scales). 
Then, make this type an instance of 'IsConnective' ; this requires providing a 'ConnectiveDesc' value, which has three fields: 

   - 'symbol' which is used for display.
   - 'priority' which is an integer value representing operator prioriy (used in deciding where to place parenthesis when displaying).
   - 'fun' which is used for evaluation: it is a function that take two booleans corresponding to each operand and returns one. 

@
instance IsConnective MyOp where
    -- the argument is there for type reason, it contains no information whatsoever
    conn _ = QuantifierDesc {
         symbol = "xor"
       , priority = 2
         -- defining an "xor" interpretation
       , fun = (/=) 
    }
@

Then the formula "p xor q" may be defined as below (for explanation on how this comes together, cf 'Exh.Formula.Internal')

@
MkF $ Formula_ [p, q] (Op @MyOp)
@

That's very cumbersome, as it exposes internal gears we don't want to see. We can camouflage this by defining the following auxiliary:

@
(<+>) :: Formula -> Formula -> Formula
(<+>) p q = MkF $ Formula_ [p, q] $ Op @MyOp
@

With this, "p xor q" may be written as:

@
p <+> q
@
-}
module Exh.Proposition(
      Op(..)
    , ConnectiveDesc(..)
    , Or(..)
    , And(..)
    , (<|>)
    , (&.)
    , (|.)
    , (=>.)
    , (<=>.)
    , neg
    , Neg
) where

import Control.Monad.Writer.Strict
import Data.Maybe (catMaybes)
import Data.List  (nub)
import Data.Proxy
import Data.Typeable

import Exh.Internal
import Exh.Display



------------------- BINARY CONNECTIVES -----------------

data ConnectiveDesc = ConnectiveDesc {
      symbol   :: String               -- ^ displayed between operands
    , priority :: Int                  -- ^ operators get parenthesized if themselves operand of a lower priority operator (e.g. AND p (OR q r) = p & (q | r))
    , fun      :: Bool -> Bool -> Bool -- ^ how to combine operands to get the truth-value of the whole
}

data Or = Or
instance IsConnective Or where
    conn _ = ConnectiveDesc {
          symbol   = "???"
        , priority = 3
        , fun      = (||)
    }

data And = And
instance IsConnective And where
    conn _ = ConnectiveDesc {
          symbol   = "???"
        , priority = 2
        , fun      = (&&)
    }

-- | Associated type constant.
class (Typeable op) => IsConnective op where
    conn :: Proxy op -> ConnectiveDesc


data Op op = Op
deriving instance (IsConnective op) => Eq (Op op)

getConn :: forall op. (IsConnective op) => Op op -> ConnectiveDesc
getConn _ = conn (Proxy :: Proxy op)

replaceOp :: (IsConnective op1, IsConnective op2) => Formula_ (Op op1) -> Formula_ (Op op2)
replaceOp (Formula_ children _) = Formula_ children Op

replaceOpAs :: (IsConnective op1, IsConnective op2) => f op2 -> Formula_ (Op op1) -> Formula_ (Op op2)
replaceOpAs = const replaceOp

instance (IsConnective op) => IsFormula (Op op) where

    display_ op ((priority1, f1):(priority2, f2):[]) = 
        let ConnectiveDesc {..} = getConn op in 
            (,) priority $ execWriter $ do
            tell $ parenthesizeIf (priority1 > priority) f1
            tell " "
            tell symbol
            tell " "
            tell $ parenthesizeIf (priority2 > priority) f2
    display_ _ _ = error "Wrong arity"

    evaluate_ g (Formula_ {..}) = 
        let p:q:[] = children in do
        let ConnectiveDesc {..} = getConn userData
        result1 <- evaluate g p
        result2 <- evaluate g q
        return $ result1 `fun` result2

    alts_ sg@ScaleGen{..} Formula_{..} = let
        p:q:[] = children
        !altsP = alts sg p -- forcing nubbing of the children at least
        !altsQ = alts sg q -- forcing nubbing of the children at least
        altsSameOp = 
            [ Formula_ [altP, altQ] (Op @op)
            | altP  <- alts sg p 
            , altQ  <- alts sg q ]

        scalarAlts = catMaybes
            [ applyScale altSameRoot scale
            | altSameRoot <- altsSameOp
            , scale       <- _opScales  ]

        subAlts
            | _subst = altsP ++ altsQ
            | otherwise = []

        in nub $ (map MkF altsSameOp) ++ scalarAlts ++ subAlts

instance (Typeable op1, Typeable op2) => MapUserData (Op op1) (Op op2) where
    mapUserData Op = Op

-- | Form a scale from two operator types.
(<|>) :: forall op1 op2. (IsConnective op1, IsConnective op2) => op1 -> op2 -> Scale
(<|>) _ _ = Scale (Proxy @(Op op1)) (Proxy @(Op op2))


infixr 3 &.
infixr 2 |.
-- | Or and And operators
(&.), (|.) :: Formula -> Formula -> Formula
(&.) f g = MkF $ Formula_ [f, g] (Op @And) 
(|.) f g = MkF $ Formula_ [f, g] (Op @Or)  


data Implication = Implication
instance IsConnective Implication where
    conn _ = ConnectiveDesc {
          symbol   = "???"
        , priority = 2
        , fun      = (<=)
    }


data Equiv = Equiv
instance IsConnective Equiv where
    conn _ = ConnectiveDesc {
          symbol   = "???"
        , priority = 2
        , fun      = (==)
    }


(=>.), (<=>.) :: Formula -> Formula -> Formula
(=>.)  f g = MkF $ Formula_ [f, g] (Op @Implication) 
(<=>.) f g = MkF $ Formula_ [f, g] (Op @Equiv)  


------------------- NEGATION -----------------

data Neg = Neg deriving (Eq)

priorityNeg :: Int
priorityNeg = 1

instance IsFormula Neg where

    display_ _ ((priority, f):[]) = (priorityNeg,  '??':parenthesizeIf (priority > priorityNeg) f)
    display_ _ _ = error "Negation can only have one child."

    evaluate_ g (Formula_ [f] _) = not <$> evaluate g f
    evaluate_ _ _ = error "Negation can only have one child."

    alts_ sg (Formula_ [child] _) = neg <$> alts sg child
    alts_ _ _ = error "Negation can only have one child."

-- | Negates a formula.
neg :: Formula -> Formula
neg f = MkF $ Formula_ [f] Neg 