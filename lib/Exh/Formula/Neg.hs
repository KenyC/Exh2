module Exh.Formula.Neg(
      Neg(..)
    , neg
) where

import Exh.Formula.Internal
import Exh.Formula.Utils

data Neg = Neg deriving (Eq)

priorityNeg :: Int
priorityNeg = 1

instance IsFormula Neg where

    display_ _ ((priority, f):[]) = (priorityNeg,  '¬':parenthesizeIf (priority > priorityNeg) f)
    display_ _ _ = error "Negation can only have one child."

    evaluate_ g (Formula_ [f] _) = not <$> evaluate g f
    evaluate_ _ _ = error "Negation can only have one child."

    alts_ sg (Formula_ [child] _) = neg <$> alts sg child
    alts_ _ _ = error "Negation can only have one child."

neg :: Formula -> Formula
neg f = MkF $ Formula_ [f] Neg 