module Formula where

import Control.Monad.Writer.Strict
import Data.String
import Data.Traversable (for)
import Data.Map (Map)
import qualified Data.Map as Map

------------------- TYPES -----------------

data EvalError 
    = NoValueFor AtomName
    deriving (Show, Eq)


------------------- GENERIC FORMULA CONSTRUCT -----------------

class IsFormula f where
    children  :: f -> [Formula]
    display_  :: f -> [(Int, String)] -> (Int, String)
    evaluate_ :: Assignment Bool -> f -> Either EvalError Bool

data Formula where
    MkF :: (IsFormula f) => f -> Formula


instance IsFormula Formula where
    children  (MkF f) = children  f 
    display_  (MkF f) = display_  f 
    evaluate_ g (MkF f) = evaluate_ g f

------------------- UTILS -----------------

parenthesize :: String -> String
parenthesize body = join $ 
    [ "("
    , body
    , ")" ]

parenthesizeIf :: Bool -> String -> String
parenthesizeIf True  = parenthesize
parenthesizeIf False = id

------------------- DISPLAY -----------------

displayAux :: (IsFormula f) => f -> (Int, String)
displayAux f = display_ f (map displayAux $ children f)

display :: (IsFormula f) => f -> String
display f = snd $ displayAux f

------------------- EVALUATE -----------------

evaluate :: Assignment Bool -> Formula -> Either EvalError Bool
evaluate = evaluate_

evalMulti :: (Traversable f) => f (Assignment Bool) -> Formula -> Either EvalError (f Bool)
evalMulti container formula = for container $ \g -> evaluate g formula

------------------- ASSIGNMENTS -----------------

type Assignment t = Map AtomName t

-- | list is supposed to not contain twice the same name
fullLogicalSpace :: [AtomName] -> [Assignment Bool]
fullLogicalSpace names = 
    [Map.fromList $ zip names bools | bools <- truthTable] where
    -- using list monad, we generate all combinations of (True, False)
    truthTable = mapM (const $ [False, True]) names 


------------------- ATOM -----------------

newtype AtomName = AtomName String deriving (Show, Eq, Ord, Read, IsString)
data Atom = Atom AtomName 

instance IsFormula Atom where
    children _ = []
    display_ (Atom (AtomName nameStr)) _ = (0, nameStr)
    evaluate_ g (Atom name) = maybe 
                                (Left $ NoValueFor name) 
                                Right $
                                Map.lookup name g 


-- | syntactic sugar
atom :: AtomName -> Formula
atom = MkF. Atom 


------------------- BINARY CONNECTIVES -----------------

data BinaryConnective = BinaryConnective {
      symbol   :: String
    , priority :: Int
    , fun      :: Bool -> Bool -> Bool
}

orConnective :: BinaryConnective
orConnective = BinaryConnective {
      symbol   = "∨"
    , priority = 3
    , fun      = (||)
}

andConnective :: BinaryConnective
andConnective = BinaryConnective {
      symbol   = "∧"
    , priority = 2
    , fun      = (&&)
}



data Op = Op BinaryConnective Formula Formula

instance IsFormula Op where
    children (Op _ f g) = [f, g] 

    display_ (Op BinaryConnective{..} _ _) ((priority1, f1):(priority2, f2):[]) = (,) priority $ execWriter $ do
        tell $ parenthesizeIf (priority1 > priority) f1
        tell " "
        tell symbol
        tell " "
        tell $ parenthesizeIf (priority2 > priority) f2
    display_ _ _ = error "Wrong arity"

    evaluate_ g (Op BinaryConnective{..} p q) = do
        result1 <- evaluate_ g p
        result2 <- evaluate_ g q
        return $ result1 `fun` result2

infixr 3 .&
infixr 2 .|
-- | syntactic sugar
(.&), (.|) :: Formula -> Formula -> Formula
(.&) f g = MkF $ Op andConnective f g
(.|) f g = MkF $ Op orConnective  f g

------------------- NEGATION -----------------

data Neg = Neg Formula

priorityNeg :: Int
priorityNeg = 1

instance IsFormula Neg where
    children (Neg f) = [f]

    display_ _ ((priority, f):[]) = (priorityNeg,  '¬':parenthesizeIf (priority > priorityNeg) f)

    evaluate_ g (Neg f) = not <$> (evaluate_ g f)

neg :: Formula -> Formula
neg = MkF . Neg