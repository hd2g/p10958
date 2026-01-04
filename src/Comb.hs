module Comb where

import Prelude

operators :: [Char]
operators = ['+', '-', '*', '/', '^', ' ']

sourceAsc :: String
sourceAsc = "1 2 3 4 5 6 7 8 9"

allCombinations :: String -> [String]
allCombinations s = combine source
  where
    source :: [String]
    source = words s

    combine :: [String] -> [String]
    combine [] = []
    combine [x] = [x]
    combine (x:xs) =
      [ unary ++ x ++ (if op == ' ' then [] else [op]) ++ rest
      | op <- operators
      , unary <- ["", "-"]
      , rest <- combine xs
      ]
