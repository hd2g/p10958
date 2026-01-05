module Comb where

import Prelude

import Data.Either (either, rights)
import Data.List ((!?))
import Data.Text qualified as T

import Operator
import Parser

operators :: [Char]
operators = ['+', '-', '*', '/', '^', ' ']

-- REFACTOR: Replace type to Text
sourceAsc :: String
sourceAsc = "1 2 3 4 5 6 7 8 9"

-- REFACTOR: Replace type to Text
allCombinations :: String -> [String]
allCombinations s = concatMap bracketify $ combine source
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

bracketify :: String -> [String]
bracketify s = either (const []) id $ map showExpr <$> generateBracketingsWithUnary <$> classify (T.pack s)

toBinaryOp :: String -> BinaryOp
toBinaryOp "+" = Add
toBinaryOp "-" = Sub
toBinaryOp "*" = Mul
toBinaryOp "/" = Div
toBinaryOp "^" = Pow
toBinaryOp _ = error "Unknown operator"

generateBracketingsWithUnary :: ([Double], [String]) -> [Expr]
generateBracketingsWithUnary ([x], []) = [Number x]
generateBracketingsWithUnary ([x], (op:_)) = [Unary (toUnaryOp op) (Number x)]
generateBracketingsWithUnary (xs, ops) =
  let n = length xs
      splitPoints = [0 .. n - 1]
      unaryExprs = case ops of
                     (u:_) | u `elem` ["+", "-"] ->
                       [Unary (toUnaryOp u) e | e <- generateBracketingsWithUnary (xs, (tail ops))]
                     _ -> []
  in unaryExprs ++
     concatMap (\i ->
       let left = take i xs
           right = drop i xs
           leftOps = take (i - 1) ops
           rightOps = drop i ops
           currentOp = ops !? (i - 1)
           leftExprs = generateBracketingsWithUnary (left, leftOps)
           rightExprs = generateBracketingsWithUnary (right, rightOps)

       in case currentOp of
         Just op -> [Binary (toBinaryOp op) le re | le <- leftExprs, re <- rightExprs]
         Nothing -> []
      ) splitPoints

toUnaryOp :: String -> UnaryOp
toUnaryOp "+" = Plus
toUnaryOp "-" = Minus
toUnaryOp _ = error "Unknown unary operator"

-- classify :: Text -> Either (ParseErrorBundle Text Void) ([Double], [String])
classify input = do
  expr <- parseExpr input
  let (nums, ops) = extractNumsAndOpsWithUnary expr
  return (nums, map showBinaryOp ops)
  where
    extractNumsAndOpsWithUnary :: Expr -> ([Double], [BinaryOp])
    extractNumsAndOpsWithUnary (Number n) = ([n], [])
    extractNumsAndOpsWithUnary (Unary Minus e) =
      let (nums, ops) = extractNumsAndOpsWithUnary e
      in (map (* (-1)) nums, ops)
    extractNumsAndOpsWithUnary (Unary Plus e) = extractNumsAndOpsWithUnary e
    extractNumsAndOpsWithUnary (Binary op e1 e2) =
      let (nums1, ops1) = extractNumsAndOpsWithUnary e1
          (nums2, ops2) = extractNumsAndOpsWithUnary e2
      in (nums1 ++ nums2, ops1 ++ ops2 ++ [op])
