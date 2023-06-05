module Main where

import Prelude

import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators as Parsing.Combinators
import Parsing.String as Parsing.String
import Parsing.String.Basic as Parsing.String.Basic
import Control.Alt ((<|>))

data BoolStr
  = TrueStr String
  | FalseStr String

pTrue :: Parser String BoolStr
pTrue = do
  val <- Parsing.String.Basic.oneOf [ 't', 'y', '1' ]
  pure $ TrueStr val

pFalse :: Parser String BoolStr
pFalse = do
  val <- Parsing.String.Basic.oneOf [ 'f', 'n', '0' ]
  pure $ FalseStr val

-- pTrue :: Parser String Char
-- pTrue = Parsing.String.Basic.oneOf [ 't', 'y', '1' ]

-- pFalse :: Parser String Char
-- pFalse = Parsing.String.Basic.oneOf [ 'f', 'n', '0' ]

-- pTest :: Parser String String
-- pTest = Parsing.String.string "yes"

pBoolean :: Parser String Boolean
pBoolean = do
  _ <- pTrue <|> pFalse
  Parsing.String.eof
  pure true

-- Parsing.Combinators.try (pTrue $> true <|> pFalse $> false)

-- parse :: String -> Either String Boolean
-- parse input = do
--   pTrue $> true <|> pFalse $> false

-- main :: Effect Unit
main = do
  Parsing.runParser "y" pBoolean
