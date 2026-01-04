module Main (main) where

import Prelude

import Data.Foldable (for_)
import Text.Printf (printf)

import Comb (allCombinations, sourceAsc)
import Solve (solve, found)

distFilePath :: FilePath
distFilePath = "./out.txt"

main :: IO ()
main = do
  let inputs = allCombinations sourceAsc
  for_ (zip inputs (solve inputs))  $ \ (input, result) -> do
    appendFile distFilePath $ printf "%s\t%f\n" (input :: String) (result :: Double)
