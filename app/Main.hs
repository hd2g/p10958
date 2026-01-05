module Main (main) where

import Prelude

import Data.Foldable (for_)
import Text.Printf (printf)

import Comb (allCombinations, sourceAsc)
import Solve (solve, found)

main :: IO ()
main = do
  let inputs = allCombinations sourceAsc
  for_ (zip inputs (solve inputs))  $ \ (input, result) -> do
    putStrLn $ printf "%s\t%f" (input :: String) (result :: Double)
