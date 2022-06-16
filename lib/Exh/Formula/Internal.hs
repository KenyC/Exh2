{-# LANGUAGE ConstraintKinds #-}
module Exh.Formula.Internal(
      EvalError(..)
    , IsFormula(..)
    , Formula(..)

    -- useful Internal stuff
    , Formula_(..)
    , matching
    , getData
    , foldFormula

    -- high-level stuff
    , alts
    , getAtoms
    , display
    , evaluate
    , evalMulti
    , AtomName(..)
    , Assignment(..)
    , ScaleGen(..)
    , MapUserData(..)
    , replaceFormula
    , Scale(..)
    , applyScale
) where

import Control.Monad.Writer.Strict
import Data.String
import Data.Proxy
import Data.Traversable (for)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable

------------------- TYPES -----------------

data EvalError 
    = NoValueFor AtomName
    deriving (Show, Eq)


------------------- GENERIC FORMULA CONSTRUCT -----------------

class (Typeable f, Eq f) => IsFormula f where
    display_  :: f -> [(Int, String)] -> (Int, String)
    evaluate_ :: Assignment Bool -> Formula_ f -> Either EvalError Bool
    alts_     :: ScaleGen -> Formula_ f -> [Formula]

    {-# MINIMAL display_, evaluate_, alts_ #-}

    getAtoms_ :: Formula_ f -> Set AtomName
    getAtoms_ Formula_{..} = Set.unions [getAtoms_ f | MkF f <- children] 

data Formula_ a = Formula_ {
    children :: [Formula]
  , userData :: a
} deriving (Eq)

data Formula where
    MkF :: (IsFormula f) => Formula_ f -> Formula


matching :: (IsFormula f) => Formula -> Maybe (Formula_ f)
matching (MkF f) = cast f 

getData :: (IsFormula f) => Formula -> Maybe f
getData =  matching >=> (Just . userData)

instance Eq Formula where
    (==) (MkF f) (MkF g) = (cast f) == (Just g)
-- data Test (k :: * -> Constraint) where
--     MkT :: (IsFormula f, k f) => f -> Test k

-- type DblConst a = (Show a, Read a)
-- type Test1 = Test DblConst

instance Show Formula where
    show f = display f

foldFormula :: (forall f. (IsFormula f, Typeable f, Eq f) => f -> [a] -> a) -> Formula -> a
foldFormula combine (MkF (Formula_ {..})) = combine userData $ map (foldFormula combine) children 

-- instance IsFormula Formula where
--     display_      (MkF f) = display_  f 
--     evaluate_ g   (MkF f) = evaluate_ g f
--     alts_     sg  (MkF f) = alts_ sg f

alts :: ScaleGen -> Formula -> [Formula]
alts sg (MkF f) = alts_ sg f

getAtoms :: Formula -> Set AtomName
getAtoms (MkF f) = getAtoms_ f

------------------- DISPLAY -----------------

displayAux :: (IsFormula f) => Formula_ f -> (Int, String)
displayAux f = display_ (userData f) [displayAux child | MkF child <- children f]

display :: Formula -> String
display (MkF f) = snd $ displayAux f

------------------- EVALUATE -----------------

evaluate :: Assignment Bool -> Formula -> Either EvalError Bool
evaluate g (MkF f) = evaluate_ g f

evalMulti :: (Traversable f) => f (Assignment Bool) -> Formula -> Either EvalError (f Bool)
evalMulti container formula = for container $ \g -> evaluate g formula

------------------- ASSIGNMENTS -----------------

newtype AtomName = AtomName String deriving (Show, Eq, Ord, Read, IsString)
type Assignment t = Map AtomName t


------------------- SCALE -----------------

data ScaleGen = ScaleGen {
    _opScales :: [Scale]
  , _subst :: Bool
} deriving (Eq)

-- instance Default ScaleGen where
--     def = ScaleGen {
--         _opScales = [Or <|> And]
--       , _subst = True
--     }

class (Typeable t1, Typeable t2) => MapUserData t1 t2 where
    mapUserData :: t1 -> t2

replaceFormula :: (MapUserData t1 t2) => Formula_ t1 -> Formula_ t2
replaceFormula (Formula_ children userData') = 
    Formula_{..} where
    userData = mapUserData userData'


data Scale where
    Scale :: 
           ( Typeable op1
           , Typeable op2
           , Eq op1
           , Eq op2
           , IsFormula op1
           , IsFormula op2
           , MapUserData op1 op2)
        => Proxy op1
        -> Proxy op2
        -> Scale

instance Eq Scale where
    (==) (Scale op1 op2) (Scale op1' op2') = 
        (typeRep op1 == typeRep op1') && 
        (typeRep op2 == typeRep op2')



-- op1 ~ op1'
-- => f op1 -> f op1'

applyScale :: (IsFormula op1, Typeable op1, Eq op1) => Formula_ op1 -> Scale -> Maybe Formula
applyScale f (Scale op1' op2) = let 
    mapFrom ::
        forall op1 op1' op2.
       ( MapUserData op1' op2
       , Typeable op1 
       , Typeable op1'
       , Typeable op2) 
       => Formula_ op1 
       -> Proxy op1' 
       -> Proxy op2
       -> Maybe (Formula_ op2)
    mapFrom g _ _ = fmap (replaceFormula @op1' @op2) $ (cast @(Formula_ op1) @(Formula_ op1') g)

    in MkF <$> mapFrom f op1' op2
