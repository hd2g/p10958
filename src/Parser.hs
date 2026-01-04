module Parser where

import Prelude hiding (div, exponent)

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Void (Void)
import Data.Text (Text)
-- import Data.Functor (void)

import Operator

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

integer :: Parser Double
integer = lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

unaryOp :: Parser UnaryOp
unaryOp = plus <|> minus
  where
    plus = Plus <$ symbol "+"
    minus = Minus <$ symbol "-"

binaryOp :: Parser BinaryOp
binaryOp = add <|> sub <|> mul <|> div <|> pow
  where
    add = Add <$ symbol "+"
    sub = Sub <$ symbol "-"
    mul = Mul <$ symbol "*"
    div = Div <$ symbol "/"
    pow = Pow <$ symbol "^"

expr :: Parser Expr
expr = makeExprParser term operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ InfixR (Binary Pow <$ symbol "^") ]
  , [ InfixL (Binary Mul <$ symbol "*")
    , InfixL (Binary Div <$ symbol "/")
    ]
  , [ InfixL (Binary Add <$ symbol "+")
    , InfixL (Binary Sub <$ symbol "-")
    ]
  ]

term :: Parser Expr
term = unary <|> number <|> parens expr

unary :: Parser Expr
unary = do
  op <- unaryOp
  e <- term
  return $ Unary op e

number :: Parser Expr
number = Number <$> integer

parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr input = runParser (spaceConsumer >> expr) "" input
