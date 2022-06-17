{-|
This module gives various utilities to inspect the truth-conditions of formulas, including:

  - utility for creating truth tables
  - utility for building all possible assignments
  - 'equivalent' or '<=>' checks equivalence between two formulas
  - 'entails' checks if one formula entails another
  - 'compatible' checks if two formulas can both be satisfied in some assignment (aka consistent with one another)
  - 'independent' checks if two formulas are logically independent

Each function has an "on" that allows to check against a custom set of assignments, rather than the whole logical space ('compatibleOn', 'entailsOn', 'equivalentOn', 'independent')
-}
module Exh.Semantics(
    fullLogicalSpace
  , equivalentOn
  , entailsOn
  , independentOn
  , compatibleOn
  , equivalent
  , entails
  , independent
  , compatible
  , mkTruthTable
  , mkTruthTableSafe
  , TruthTable
  , displayTruthTable
  , (<=>)
) where

import Control.Monad.Writer.Strict
import qualified Data.Map as Map
import qualified Data.Set as Set

import Exh.Formula.Internal
import Exh.Formula.Atom
import Exh.Display

-- | Returns all possible assignments of truth values to the given set of names.
--   No commitment to any behavior if same name appears twice in the list.
fullLogicalSpace :: [AtomName] -> [Assignment Bool]
fullLogicalSpace names = 
    [Map.fromList $ zip names bools | bools <- truthTable] where
    -- using list monad, we generate all combinations of (True, False)
    truthTable = mapM (const [False, True]) names 



-- | Check if the two formulas yield the same truth value on all assignments provided.
equivalentOn :: [Assignment Bool] -> Formula -> Formula -> Either EvalError Bool
equivalentOn u f1 f2 = do
    r1 <- evalMulti u f1
    r2 <- evalMulti u f2
    return $ r1 == r2

-- | Check if, across the assignments provided, 'f2' is always true when 'f1' is.
entailsOn :: [Assignment Bool] -> Formula -> Formula -> Either EvalError Bool
entailsOn u f1 f2 = do
    r1 <- evalMulti u f1
    r2 <- evalMulti u f2
    return $ r1 <= r2

-- | Check if, on the set of assignments provided, 'f2' does not entail 'f1' and 'f1' does not entail f2.
independentOn :: [Assignment Bool] -> Formula -> Formula -> Either EvalError Bool
independentOn u f1 f2 = all not <$> sequence [entailsOn u f1 f2, entailsOn u f2 f1]

-- | Check if one of the assignments provided makes both formulas true.
compatibleOn :: [Assignment Bool] -> Formula -> Formula -> Either EvalError Bool
compatibleOn u f1 f2 = do
    r1 <- evalMulti u f1
    r2 <- evalMulti u f2
    return $ any id $ zipWith (&&) r1 r2

{-# INLINE toAbsolute #-}
toAbsolute :: ([Assignment Bool] -> Formula -> Formula -> Either EvalError Bool) -> Formula -> Formula -> Either EvalError Bool
toAbsolute fun f1 f2 = fun u f1 f2 where u = fullLogicalSpace (Set.toList $ (getAtoms f1) `Set.union` (getAtoms f2)) 

-- | Same as 'equivalentOn' but using all possible assignments.
infix 1 <=> 
equivalent, (<=>) :: Formula -> Formula -> Either EvalError Bool
equivalent = toAbsolute equivalentOn
(<=>) = equivalent

-- | Same as 'entailsOn' but using all possible assignments.
entails :: Formula -> Formula -> Either EvalError Bool
entails = toAbsolute entailsOn


-- | Same as 'independentOn' but using all possible assignments.
independent :: Formula -> Formula -> Either EvalError Bool
independent = toAbsolute independentOn

-- | Same as 'compatibleOn' but using all possible assignments.
compatible :: Formula -> Formula -> Either EvalError Bool
compatible = toAbsolute compatibleOn


------------------- TRUTH TABLE -----------------

-- | Truth table for a set of formulas ; can be printed.
data TruthTable = TruthTable {
    headers      :: [Formula]
  , atoms        :: [AtomName]
  , assignments  :: [Assignment Bool]
  , outcomes     :: [[Bool]] -- ^ `outcomes !! 34` gives you the result of evaluating the formulas with `assignments !! 34`
} deriving (Eq)

instance Show TruthTable where
    show tt = displayTruthTable tt 


mkTruthTableSafe :: [Formula] -> Either EvalError TruthTable
mkTruthTableSafe fs = do
    let atoms   = Set.toList $ Set.unions $ map getAtoms fs
    let headers = fs
    let assignments = fullLogicalSpace atoms
    outcomes <- forM assignments $ \assignment ->
                  forM fs $ \f ->
                    evaluate assignment f
    return TruthTable{..}

-- | Creates a truth-table from a list of formulas.
mkTruthTable :: [Formula] -> TruthTable
mkTruthTable fs = case mkTruthTableSafe fs of
    Left err -> error $ show err
    Right r  -> r

-- | Makes a truth-table into a displayable 'String'.
displayTruthTable :: TruthTable -> String
displayTruthTable TruthTable{..} = execWriter $ do
    let headerNames = map show headers
        outcomeColWidths = map ((max 3) . length) headerNames
        atomNames = [name | AtomName name <- atoms] 
        atomColWidths = map ((max 3) . length) atomNames

    -- first line
    tell "|"
    forM_ (zip atomNames atomColWidths) $ \(atomName, sizeCol) -> do
        tell $ centerIn sizeCol atomName
        tell "|"

    forM_ (zip headerNames outcomeColWidths) $ \(headerName, sizeCol) -> do
        tell $ centerIn sizeCol headerName
        tell "|"
    tell "\n"

    -- separation line
    let nSpaces = sum atomColWidths + sum outcomeColWidths + length atomNames + length headerNames + 1
    tell $ replicate nSpaces '-'
    tell "\n"

    let toStr :: Bool -> String
        toStr True  = "1"
        toStr False = "0"

    forM_ (zip assignments outcomes) $ \(assignment, outcomeLine) -> do
        tell "|"
        forM_ (zip atoms atomColWidths) $ \(atom, sizeCol) -> do
            tell $ centerIn sizeCol $ toStr $ assignment Map.! atom
            tell "|"

        forM_ (zip outcomeLine outcomeColWidths) $ \(outcome, sizeCol) -> do
            tell $ centerIn sizeCol $ toStr outcome
            tell "|"

        tell "\n"

    
    return ()
