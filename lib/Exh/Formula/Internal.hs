{-# LANGUAGE ConstraintKinds #-}
{-|
This module defines the 'Formula' type and various utilities around it. 

The construction is as follows: a formula is a list of children formula (potentially empty) and some user data. In concrete terms, 'Formula' wraps @Formula_ t@ where @t@ is the type of the user data, and @Formula_ t@ is a record containing 'children' and 'userData'. 

@t@ must implement 'IsFormula'. Any type that implements 'IsFormula' must *a minima* specify how to evaluate @Formula_ t@ against an assignment (cf 'evaluate_'), how to compute alternatives to @Formula_ t@ (cf 'alts_'), and how to display it, given display values for the children.
-}
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
    , freeVars
    , display
    , evaluate
    , evalMulti
    , AtomName(..)
    , VarName(..)
    , Atom(..)
    , Assignment(..)
    , (~>)
    , (=@)
    , mkG
    , modG
    , ScaleGen(..)
    , MapUserData(..)
    , replaceFormula
    , Scale(..)
    , applyScale
) where

import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Data.Default
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
    = NoValueFor AtomName      -- ^ the formula contains atom p but the assignment does not a have a value for p.
    | UnvaluedVar VarName      -- ^ trying to get a value for variable x but the assignment does not contain a value for x.
    deriving (Show, Eq)


------------------- GENERIC FORMULA CONSTRUCT -----------------

class (Typeable f, Eq f) => IsFormula f where
    display_  :: f -> [(Int, String)] -> (Int, String)
    evaluate_ :: Assignment -> Formula_ f -> Either EvalError Bool
    alts_     :: ScaleGen -> Formula_ f -> [Formula]

    {-# MINIMAL display_, evaluate_, alts_ #-}

    getAtoms_ :: Formula_ f -> Set Atom
    getAtoms_ Formula_{..} = Set.unions [getAtoms_ f | MkF f <- children] 

    freeVars_ :: Formula_ f -> Set VarName
    freeVars_ Formula_{..} = Set.unions [freeVars_ f | MkF f <- children] 

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

getAtoms :: Formula -> Set Atom
getAtoms (MkF f) = getAtoms_ f

freeVars :: Formula -> Set VarName
freeVars (MkF f) = freeVars_ f

------------------- DISPLAY -----------------

displayAux :: (IsFormula f) => Formula_ f -> (Int, String)
displayAux f = display_ (userData f) [displayAux child | MkF child <- children f]

display :: Formula -> String
display (MkF f) = snd $ displayAux f

------------------- EVALUATE -----------------

-- | Evaluate a formula against an assignment. May return `Left err` if assignment does not give values for all propositions in a formula.
evaluate :: Assignment -> Formula -> Either EvalError Bool
evaluate g (MkF f) = evaluate_ g f

-- | Evaluate a formula against a set/list/etc of assignments. Returns an error if any evaluation fails.
evalMulti :: (Traversable f) => f Assignment -> Formula -> Either EvalError (f Bool)
evalMulti container formula = for container $ \g -> evaluate g formula

------------------- ASSIGNMENTS -----------------

-- | Name for propositional atoms. Thin wrapper around a 'String'. If -XOverloadedStrings is on, can be specified as: `name :: AtomName = "foo"`.
newtype AtomName = AtomName {getAtomName :: String} deriving (Show, Eq, Ord, Read, IsString)
data Atom = Atom {
    arity :: !Int 
  , name  :: !AtomName 
} deriving (Ord, Eq, Show)
newtype VarName  = VarName String deriving (Show, Eq, Ord, Read, IsString)
data Assignment = Assignment {
    varVals  :: Map VarName Int
  , atomVals :: Map Atom (Map [Int] Bool)
} deriving (Eq, Show)

instance Default Assignment where
    def = Assignment Map.empty Map.empty

newtype AssignmentBuilder a = AssignmentBuilder {
    unwrapAssignMonoid :: State Assignment a
} deriving (Functor, Applicative, Monad, MonadState Assignment)

mkG :: AssignmentBuilder () -> Assignment
mkG = modG def

modG :: Assignment -> AssignmentBuilder () -> Assignment
modG inputAssignment builder = execState (unwrapAssignMonoid builder) inputAssignment

(=@) :: VarName -> Int -> AssignmentBuilder ()
(=@) name val = do
    modify $ \assignment ->
        assignment {varVals = Map.insert name val $ varVals assignment} 

class AssignableToAtom t where
    (~>) :: AtomName -> t -> AssignmentBuilder ()

instance AssignableToAtom Bool where
    (~>) name val = do
        modify $ \assignment ->
            assignment {atomVals = Map.insert newAtom newVal $ atomVals assignment} where
            newAtom = Atom 0 name
            newVal  = Map.fromList [([], val)] 

instance AssignableToAtom [Bool] where
    (~>) name val = do
        modify $ \assignment ->
            assignment {atomVals = Map.insert newAtom newVal $ atomVals assignment} where
            newAtom = Atom {arity = 1 , name = name}
            newVal  = Map.fromList [([i], bool) | (i, bool) <- zip [0..] val] 


instance AssignableToAtom [([Int], Bool)] where
    (~>) name val = _todoAssignable3



------------------- SCALE -----------------

data ScaleGen = ScaleGen {
    _opScales :: [Scale] -- ^ scales to be used for item replacement
  , _subst :: Bool       -- ^ are children nodes alternatives to parent nodes?
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
