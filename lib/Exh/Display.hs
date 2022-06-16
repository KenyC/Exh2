module Exh.Display where

import Data.Word

centerIn :: Int -> String -> String
centerIn n s = let
    missingSpace = max 0 $ n - length s
    leftSide     = missingSpace `quot` 2
    rightSide    = missingSpace - leftSide

    in replicate leftSide ' ' ++ s ++ (replicate rightSide ' ')
