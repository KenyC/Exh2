import Test.Tasty

import qualified Test.Display
import qualified Test.Evaluate
import qualified Test.Alts
import qualified Test.Exh

main :: IO ()
main = 
    defaultMain $ 
        testGroup "tests"
            [ Test.Display.allTests 
            , Test.Evaluate.allTests 
            , Test.Alts.allTests     
            , Test.Exh.allTests      ]


