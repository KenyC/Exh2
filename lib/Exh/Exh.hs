module Exh.Exh(
      ExhOptions(..)
    , Exh(..)
    , exh
    , exhWith
    , ieExhaustify
) where

import Data.Default
import Data.List (foldl')
import Data.Traversable
import qualified Data.Set as Set
import Debug.Trace

import Exh.Formula.Internal
import Exh.Formula.Atom
import Exh.Formula.Op
import Exh.Semantics

data ExhOptions = ExhOptions {
    _scaleGen       :: ScaleGen    
  , _stipulatedAlts :: Maybe [Formula]
} deriving (Eq)

instance Default ScaleGen where
    def = ScaleGen {
        _opScales = [Or <|> And]
      , _subst    = True
    }

instance Default ExhOptions where
    def = ExhOptions {
        _scaleGen = def
      , _stipulatedAlts = Nothing
    }

data Exh = Exh {
    allAlts   :: ![Formula] 
  , ieAlts    :: ![Formula]
  , opts      :: !ExhOptions
} deriving (Eq)

instance IsFormula Exh where

    display_ _ ((_, prejacent):[]) = (0, "Exh(" ++ prejacent ++ ")")
    display_ _ _ = error "Exh can only have one child"

    evaluate_ g (Formula_ [prejacent] Exh{..}) = do
        v1 <- evaluate g prejacent
        vs <- forM ieAlts $ evaluate g
        return $! v1 && all not vs
    evaluate_ _ _ = error "Exh can only have one child"

    -- Potentially an error here: what guarantees that alts were constructed the same way as we're about to compute alts now
    alts_ _ f = 
        let Exh{..} = userData f in
        [ exhWith (opts {_stipulatedAlts = Just allAlts}) alt
        | alt <- allAlts ]


    getAtoms_ f = Set.unions $ map getAtoms $ prejacent:altsE
                  where altsE       = allAlts $ userData f
                        prejacent:_ = children f


exh = exhWith def

exhWith :: ExhOptions -> Formula -> Formula 
exhWith opts@ExhOptions{..} prejacent = let
    allAlts = case _stipulatedAlts of
        Just as -> as
        Nothing -> alts _scaleGen prejacent
    ieAlts  = ieExhaustify prejacent allAlts
    exh = Exh{..}
    in MkF $ Formula_ [prejacent] exh

data PartialOrd 
    = StrictlyGreater
    | StrictlySmaller
    | Equal
    | Incomparable
    deriving (Eq)

instance Semigroup PartialOrd where
    (<>) x y 
        | x == y = x
        | x == Equal = y 
        | y == Equal = x 
        | otherwise  = Incomparable

instance Monoid PartialOrd where
    mempty = Equal

-- | From a prejacent and a set of alternatives, returns the IE alternatives
ieExhaustify :: Formula -> [Formula] -> [Formula]
ieExhaustify prejacent alts = let
    atoms = Set.unions $ map getAtoms $ prejacent:alts
    universe = fullLogicalSpace $ Set.toList atoms
    -- can't fail the way b/c we tailor-made the universe for this formula ; 
    -- but the compiler does not know that
    Right valuesF = evalMulti universe prejacent
    restrictedUniverse = [ assignment | (assignment, True) <- zip universe valuesF]

    -- here there is potential for failure, as alternatives may be customs-specified
    -- maybe make the universe after alternatives have been computed?
    Right !evaluatedVals =  
        for restrictedUniverse $ \assignment -> 
        for alts               $ \alt ->
        not <$> evaluate assignment alt

    insertIfNotSmaller :: (a -> a -> PartialOrd) -> a -> [a] -> [a]
    insertIfNotSmaller _ x [] = [x]
    insertIfNotSmaller comp x l'@(y:l) = case comp x y of
          StrictlyGreater -> insertIfNotSmaller comp x l
          Incomparable    -> y:(insertIfNotSmaller comp x l)
          _ -> l'

    compareBools :: [Bool] -> [Bool] -> PartialOrd
    compareBools l1 l2 = 
        mconcat $ zipWith (\x y -> fromOrdering $ compare x y) l1 l2
        where fromOrdering EQ = Equal
              fromOrdering GT = StrictlyGreater
              fromOrdering LT = StrictlySmaller


    minWorlds :: [[Bool]]
    minWorlds = foldl' (flip $ insertIfNotSmaller compareBools) [] evaluatedVals where


    -- grand conjunction
    ieAltsBool :: [Bool]
    ieAltsBool = foldl' (zipWith (&&)) (repeat True) minWorlds

    ieAlts :: [Formula]
    ieAlts = [alt | (True, alt) <- zip ieAltsBool alts ] 
    in ieAlts