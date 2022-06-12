import Test.Tasty

import qualified Test.Display
import qualified Test.Evaluate

main :: IO ()
main = 
    defaultMain $ 
        testGroup "tests"
            [ Test.Display.allTests 
            , Test.Evaluate.allTests ]


