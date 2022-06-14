{-# LANGUAGE ConstraintKinds #-}
module Exh.Formula where

import Control.Monad.Writer.Strict
import Data.String
import Data.Set (Set)
import qualified Data.Set as Set
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
    display_  :: f -> [(Int, String)] -> (Int, String)
    evaluate_ :: Assignment Bool -> Formula_ f -> Either EvalError Bool
    alts_     :: ScaleGen -> Formula_ f -> [Formula]

data Formula_ a = Formula_ {
    children :: [Formula]
  , userData :: a
} deriving (Eq)

data Formula where
    MkF :: (IsFormula f, Eq f, Typeable f) => Formula_ f -> Formula


matching :: (IsFormula f, Typeable f) => Formula -> Maybe (Formula_ f)
matching (MkF f) = cast f 

getData :: (IsFormula f, Typeable f) => Formula -> Maybe f
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

type Assignment t = Map AtomName t

-- | No commitment to any behavior if same name appears twice in the list
fullLogicalSpace :: [AtomName] -> [Assignment Bool]
fullLogicalSpace names = 
    [Map.fromList $ zip names bools | bools <- truthTable] where
    -- using list monad, we generate all combinations of (True, False)
    truthTable = mapM (const $ [False, True]) names 


------------------- ATOM -----------------

newtype AtomName = AtomName String deriving (Show, Eq, Ord, Read, IsString)
data Atom = Atom AtomName deriving (Eq, Show)


instance IsFormula Atom where
    display_ (Atom (AtomName nameStr)) _ = (0, nameStr)
    evaluate_ g (Formula_ {..}) =
        let Atom name = userData in 
        maybe 
            (Left $ NoValueFor name) 
            Right $
            Map.lookup name g 
    alts_ _ f = [MkF f]


-- | syntactic sugar
atom :: AtomName -> Formula
atom = MkF . (Formula_ []) . Atom 

getAtoms :: Formula -> Set AtomName
getAtoms = foldFormula combine where
    combine x children = case cast x of
        Just (Atom name) -> Set.singleton name
        _ -> Set.unions $! children

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


infixr 3 .&
infixr 2 .|
-- | syntactic sugar
(.&), (.|) :: Formula -> Formula -> Formula
(.&) f g = MkF $ Formula_ [f, g] (Op @And) 
(.|) f g = MkF $ Formula_ [f, g] (Op @Or)  

------------------- NEGATION -----------------

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

------------------- SCALE -----------------

data ScaleGen = ScaleGen {
    _opScales :: [OpScale]
  , _subst :: Bool
} deriving (Eq)

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


applyScale :: (IsConnective op1) => Formula_ (Op op1) -> OpScale -> Maybe Formula
applyScale f (OpScale op1' op2)  
    | typeRep (userData f) == typeRep op1' = Just $ MkF $ replaceOpAs op2 f
    | otherwise = Nothing