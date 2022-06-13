module Utils where

import Test.Tasty.HUnit
import Data.List

sameElems :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Assertion
sameElems l1 l2 = 
    assertBool
        (show l1 ++ " does not have the same elements as " ++ show l2)
        (all (`elem` l1) l2 && all (`elem` l2) l1)