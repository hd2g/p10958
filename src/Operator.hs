module Operator where

import Prelude

data UnaryOp
  = Plus
  | Minus
  deriving (Show, Eq)

data BinaryOp
  = Add
  | Sub
  | Mul
  | Div
  | Pow
  deriving (Show, Eq)

data Expr
  = Number Int
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr
  deriving (Show, Eq)

showExpr :: Expr -> String
showExpr (Number n) = show n
showExpr (Unary Plus e) = "+" ++ showExpr e
showExpr (Unary Minus e) = "-" ++ showExpr e
showExpr (Binary Add lhs rhs) = "(" ++ showExpr lhs ++ " + " ++ showExpr rhs ++ ")"
showExpr (Binary Sub lhs rhs) = "(" ++ showExpr lhs ++ " - " ++ showExpr rhs ++ ")"
showExpr (Binary Mul lhs rhs) = "(" ++ showExpr lhs ++ " * " ++ showExpr rhs ++ ")"
showExpr (Binary Div lhs rhs) = "(" ++ showExpr lhs ++ " / " ++ showExpr rhs ++ ")"
showExpr (Binary Pow lhs rhs) = "(" ++ showExpr lhs ++ " ^ " ++ showExpr rhs ++ ")"

eval :: Expr -> Int
eval (Number n) = n
eval (Unary Plus e) = eval e
eval (Unary Minus e) = - (eval e)
eval (Binary Add rhs lhs) = eval rhs + eval lhs
eval (Binary Sub rhs lhs) = eval rhs - eval lhs
eval (Binary Mul rhs lhs) = eval rhs * eval lhs
eval (Binary Div rhs lhs) = eval rhs `div` eval lhs
eval (Binary Pow rhs lhs) = eval rhs ^ eval lhs

exampleExpr :: Expr
exampleExpr =
  Binary Add (Number 3) (Binary Mul (Unary Minus (Number 5)) (Number 2))

