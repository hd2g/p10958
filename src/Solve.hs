module Solve where

import Prelude

import Data.Either qualified as E
import Data.Text qualified as T

import Operator (eval)
import Parser (parseExpr)

solve :: [String] -> [Double]
solve xs = eval <$> (E.rights $ map (parseExpr . T.pack) xs)

found :: Double -> Bool
found = (== 10968)
