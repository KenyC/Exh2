module Exh.Formula.Utils(
      parenthesize
    , parenthesizeIf
) where

import Control.Monad

parenthesize :: String -> String
parenthesize body = join $ 
    [ "("
    , body
    , ")" ]

parenthesizeIf :: Bool -> String -> String
parenthesizeIf True  = parenthesize
parenthesizeIf False = id
