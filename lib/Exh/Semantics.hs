{-|
This module gives various utilities to inspect the truth-conditions of formulas, including:

  - utility for creating truth tables ('mkTruthTable')
  - utility for building all possible assignments ('fullLogicalSpace')
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
import Data.List (intersperse)

import Exh.Internal
import Exh.Atom
import Exh.Display

-- | Returns all possible assignments of truth values to the given set of names.
--   No commitment to any behavior if same name appears twice in the list.
fullLogicalSpace :: [Atom] -> [Assignment]
fullLogicalSpace atoms = do -- list monad
    -- for each atom, we need to draw boolean values 
    atomMapping <- forM atoms $ \atom -> do
        -- specifically, each atom requires as many value as 'sizeDomain' to the power of its arity
        -- e.g. p requires 1 boolean value
        --      r(x) requires 3 boolean values (r(0), r(1), r(2))
        --      s(x, y) requires 9 boolean values

        -- we use another different list monad independently to generate all possible combinations of variable values (i.e. s(0,0), s(0, 1), s(1, 0), etc.)
        let allPossibleVarAssignment = replicateM (arity atom) [0 .. sizeDomain - 1]
        -- for each combination of variable, we draw a boolean
        valuesForVarAssignment <- forM allPossibleVarAssignment $ const [False, True]
        return (atom, Map.fromList $ zip allPossibleVarAssignment valuesForVarAssignment)

    -- when evaluating at root, no variable is yet bound
    let atomVals = Map.fromList atomMapping
    let varVals  = Map.empty
    return $ Assignment {..}



-- | Check if the two formulas yield the same truth value on all assignments provided.
equivalentOn :: [Assignment] -> Formula -> Formula -> Either EvalError Bool
equivalentOn u f1 f2 = do
    r1 <- evalMulti u f1
    r2 <- evalMulti u f2
    return $ r1 == r2

-- | Check if, across the assignments provided, 'f2' is always true when 'f1' is.
entailsOn :: [Assignment] -> Formula -> Formula -> Either EvalError Bool
entailsOn u f1 f2 = do
    r1 <- evalMulti u f1
    r2 <- evalMulti u f2
    return $ all id $ zipWith (<=) r1 r2

-- | Check if, on the set of assignments provided, 'f2' does not entail 'f1' and 'f1' does not entail f2.
independentOn :: [Assignment] -> Formula -> Formula -> Either EvalError Bool
independentOn u f1 f2 = all not <$> sequence [entailsOn u f1 f2, entailsOn u f2 f1]

-- | Check if one of the assignments provided makes both formulas true.
compatibleOn :: [Assignment] -> Formula -> Formula -> Either EvalError Bool
compatibleOn u f1 f2 = do
    r1 <- evalMulti u f1
    r2 <- evalMulti u f2
    return $ any id $ zipWith (&&) r1 r2

{-# INLINE toAbsolute #-}
toAbsolute :: ([Assignment] -> Formula -> Formula -> Either EvalError Bool) -> Formula -> Formula -> Either EvalError Bool
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
  , atoms        :: [Atom]
  , assignments  :: [Assignment]
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
    let formatSatPred :: Atom -> [Int] -> String
        formatSatPred atom [] = getAtomName $ name atom
        formatSatPred atom vs = 
            (getAtomName $ name atom) ++ "(" ++ values ++ ")" where
            values = mconcat $ intersperse ", " $ map show vs


    let headerNames = map show headers
        outcomeColWidths = map ((max 3) . length) headerNames
        saturatedPredicates = 
            [ (atom, varVals) 
            | atom    <- atoms
            , varVals <- replicateM (arity atom) [0 .. sizeDomain - 1]]
        saturatedPredicatesStr = map (uncurry formatSatPred) saturatedPredicates 
        atomColWidths = map ((max 3) . length) saturatedPredicatesStr

    -- first line
    tell "|"
    forM_ (zip saturatedPredicatesStr atomColWidths) $ \(atomName, sizeCol) -> do
        tell $ centerIn sizeCol atomName
        tell "|"

    forM_ (zip headerNames outcomeColWidths) $ \(headerName, sizeCol) -> do
        tell $ centerIn sizeCol headerName
        tell "|"
    tell "\n"

    -- separation line
    let nSpaces = sum atomColWidths + sum outcomeColWidths + length atoms + length headerNames + 1
    tell $ replicate nSpaces '-'
    tell "\n"

    let toStr :: Bool -> String
        toStr True  = "1"
        toStr False = "0"

    forM_ (zip assignments outcomes) $ \(Assignment{..}, outcomeLine) -> do
        tell "|"
        forM_ (zip saturatedPredicates atomColWidths) $ \((atom, varVals), sizeCol) -> do
            tell $ centerIn sizeCol $ toStr $ atomVals Map.! atom Map.! varVals
            tell "|"

        forM_ (zip outcomeLine outcomeColWidths) $ \(outcome, sizeCol) -> do
            tell $ centerIn sizeCol $ toStr outcome
            tell "|"

        tell "\n"

    
    return ()
