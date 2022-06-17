{-|
This module defines a general interface to create binary operators, as well as useful particular instances like 'Or' and 'And'.


-}
module Exh.Formula.Op(
      Op(..)
    , BinaryConnective(..)
    , Or(..)
    , And(..)
    , (<|>)
    , (&.)
    , (|.)
) where

import Control.Monad.Writer.Strict
import Data.Maybe (catMaybes)
import Data.List  (nub)
import Data.Proxy
import Data.Typeable

import Exh.Formula.Internal
import Exh.Formula.Utils



------------------- BINARY CONNECTIVES -----------------

data BinaryConnective = BinaryConnective {
      symbol   :: String               -- ^ displayed between operands
    , priority :: Int                  -- ^ operators get parenthesized if themselves operand of a lower priority operator (e.g. AND p (OR q r) = p & (q | r))
    , fun      :: Bool -> Bool -> Bool -- ^ how to combine operands to get the truth-value of the whole
}

data Or = Or
instance IsConnective Or where
    conn _ = BinaryConnective {
          symbol   = "∨"
        , priority = 3
        , fun      = (||)
    }

data And = And
instance IsConnective And where
    conn _ = BinaryConnective {
          symbol   = "∧"
        , priority = 2
        , fun      = (&&)
    }

-- | Associated type constant.
class (Typeable op) => IsConnective op where
    conn :: Proxy op -> BinaryConnective


data Op op = Op
deriving instance (IsConnective op) => Eq (Op op)

getConn :: forall op. (IsConnective op) => Op op -> BinaryConnective
getConn _ = conn (Proxy :: Proxy op)

replaceOp :: (IsConnective op1, IsConnective op2) => Formula_ (Op op1) -> Formula_ (Op op2)
replaceOp (Formula_ children _) = Formula_ children Op

replaceOpAs :: (IsConnective op1, IsConnective op2) => f op2 -> Formula_ (Op op1) -> Formula_ (Op op2)
replaceOpAs = const replaceOp

instance (IsConnective op) => IsFormula (Op op) where

    display_ op ((priority1, f1):(priority2, f2):[]) = 
        let BinaryConnective {..} = getConn op in 
            (,) priority $ execWriter $ do
            tell $ parenthesizeIf (priority1 > priority) f1
            tell " "
            tell symbol
            tell " "
            tell $ parenthesizeIf (priority2 > priority) f2
    display_ _ _ = error "Wrong arity"

    evaluate_ g (Formula_ {..}) = 
        let p:q:[] = children in do
        let BinaryConnective {..} = getConn userData
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


