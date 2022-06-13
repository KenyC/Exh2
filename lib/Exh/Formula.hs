{-# LANGUAGE ConstraintKinds #-}
module Exh.Formula where

import Control.Monad.Writer.Strict
import Data.String
import Data.Default
import Data.Maybe (catMaybes)
import Data.List  (nub)
import Data.Proxy
import Data.Traversable (for)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import GHC.Exts (Constraint)

------------------- TYPES -----------------

data EvalError 
    = NoValueFor AtomName
    deriving (Show, Eq)


------------------- GENERIC FORMULA CONSTRUCT -----------------

class IsFormula f where
    children  :: f -> [Formula]
    display_  :: f -> [(Int, String)] -> (Int, String)
    evaluate_ :: Assignment Bool -> f -> Either EvalError Bool
    alts_     :: ScaleGen -> f -> [Formula]

data Formula where
    MkF :: (IsFormula f, Eq f, Typeable f) => f -> Formula

instance Eq Formula where
    (==) (MkF f) (MkF g) = (cast f) == (Just g)
-- data Test (k :: * -> Constraint) where
--     MkT :: (IsFormula f, k f) => f -> Test k

-- type DblConst a = (Show a, Read a)
-- type Test1 = Test DblConst

instance Show Formula where
    show f = display f

instance IsFormula Formula where
    children      (MkF f) = children  f 
    display_      (MkF f) = display_  f 
    evaluate_ g   (MkF f) = evaluate_ g f
    alts_     sg  (MkF f) = alts_ sg f

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

-- | No commitment to any behavior if same name appears twice in the list
fullLogicalSpace :: [AtomName] -> [Assignment Bool]
fullLogicalSpace names = 
    [Map.fromList $ zip names bools | bools <- truthTable] where
    -- using list monad, we generate all combinations of (True, False)
    truthTable = mapM (const $ [False, True]) names 


------------------- ATOM -----------------

newtype AtomName = AtomName String deriving (Show, Eq, Ord, Read, IsString)
data Atom = Atom AtomName deriving (Eq)


instance IsFormula Atom where
    children _ = []
    display_ (Atom (AtomName nameStr)) _ = (0, nameStr)
    evaluate_ g (Atom name) = maybe 
                                (Left $ NoValueFor name) 
                                Right $
                                Map.lookup name g 
    alts_ _ f = [MkF f]


-- | syntactic sugar
atom :: AtomName -> Formula
atom = MkF. Atom 


------------------- BINARY CONNECTIVES -----------------

data BinaryConnective = BinaryConnective {
      symbol   :: String
    , priority :: Int
    , fun      :: Bool -> Bool -> Bool
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

-- | Associated type constant 
class (Typeable op) => IsConnective op where
    conn :: Proxy op -> BinaryConnective


data Op op = Op Formula Formula
deriving instance (IsConnective op) => Eq (Op op)

getConn :: forall op. (IsConnective op) => Op op -> BinaryConnective
getConn _ = conn (Proxy :: Proxy op)

replaceOp :: (IsConnective op1, IsConnective op2) => Op op1 -> Op op2
replaceOp (Op f g) = Op f g

replaceOpAs :: (IsConnective op1, IsConnective op2) => f op2 -> Op op1 -> Op op2
replaceOpAs = const replaceOp

instance (IsConnective op) => IsFormula (Op op) where
    children (Op f g) = [f, g] 

    display_ op ((priority1, f1):(priority2, f2):[]) = 
        let BinaryConnective {..} = getConn op in 
            (,) priority $ execWriter $ do
            tell $ parenthesizeIf (priority1 > priority) f1
            tell " "
            tell symbol
            tell " "
            tell $ parenthesizeIf (priority2 > priority) f2
    display_ _ _ = error "Wrong arity"

    evaluate_ g op@(Op p q) = do
        let BinaryConnective {..} = getConn op
        result1 <- evaluate_ g p
        result2 <- evaluate_ g q
        return $ result1 `fun` result2

    alts_ sg@ScaleGen{..} op@(Op p q) = let
        !altsP = alts_ sg p -- forcing nubbing of the children at least
        !altsQ = alts_ sg q -- forcing nubbing of the children at least
        altsSameOp = 
            [ Op @op altP altQ
            | altP  <- alts_ sg p 
            , altQ  <- alts_ sg q ]

        scalarAlts = catMaybes
            [ applyScale altSameRoot scale
            | altSameRoot <- altsSameOp
            , scale       <- _opScales  ]

        subAlts
            | _subst = altsP ++ altsQ
            | otherwise = []

        in nub $ (map MkF altsSameOp) ++ scalarAlts ++ subAlts


infixr 3 .&
infixr 2 .|
-- | syntactic sugar
(.&), (.|) :: Formula -> Formula -> Formula
(.&) f g = MkF $ Op @And f g
(.|) f g = MkF $ Op @Or  f g

------------------- NEGATION -----------------

data Neg = Neg Formula deriving (Eq)

priorityNeg :: Int
priorityNeg = 1

instance IsFormula Neg where
    children (Neg f) = [f]

    display_ _ ((priority, f):[]) = (priorityNeg,  '¬':parenthesizeIf (priority > priorityNeg) f)

    evaluate_ g (Neg f) = not <$> (evaluate_ g f)

    alts_ sg (Neg f) = MkF . Neg <$> alts_ sg f

neg :: Formula -> Formula
neg = MkF . Neg

------------------- SCALE -----------------

data ScaleGen = ScaleGen {
    _opScales :: [OpScale]
  , _subst :: Bool
}

instance Default ScaleGen where
    def = ScaleGen {
        _opScales = [Or <|> And]
      , _subst = True
    }

data OpScale where
    OpScale :: 
        ( IsConnective op1
        , IsConnective op2) 
        => Proxy op1
        -> Proxy op2
        -> OpScale

instance Eq OpScale where
    (==) (OpScale op1 op2) (OpScale op1' op2') = 
        (typeRep op1 == typeRep op1') && 
        (typeRep op2 == typeRep op2')

(<|>) :: forall op1 op2. (IsConnective op1, IsConnective op2) => op1 -> op2 -> OpScale
(<|>) _ _ = OpScale (Proxy @op1) (Proxy @op2)


applyScale :: (IsConnective op1) => Op op1 -> OpScale -> Maybe Formula
applyScale f (OpScale op1' op2)  
    | typeRep f == typeRep op1' = Just $ MkF $ replaceOpAs op2 f
    | otherwise = Nothing