import Test.Tasty

import qualified Test.Display
import qualified Test.Evaluate
import qualified Test.Alts

main :: IO ()
main = 
    defaultMain $ 
        testGroup "tests"
            [ Test.Display.allTests 
            , Test.Evaluate.allTests 
            , Test.Alts.allTests     ]


