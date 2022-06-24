module Exh.Display(
    centerIn
  , parenthesize
  , parenthesizeIf
) where


centerIn :: Int -> String -> String
centerIn n s = let
    missingSpace = max 0 $ n - length s
    leftSide     = missingSpace `quot` 2
    rightSide    = missingSpace - leftSide

    in replicate leftSide ' ' ++ s ++ (replicate rightSide ' ')


parenthesize :: String -> String
parenthesize body = mconcat $ 
    [ "("
    , body
    , ")" ]

parenthesizeIf :: Bool -> String -> String
parenthesizeIf True  = parenthesize
parenthesizeIf False = id
