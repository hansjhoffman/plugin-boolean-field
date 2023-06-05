module Main (parse) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators ((<?>))
import Parsing.Combinators as Parsing.Combinators
import Parsing.String as Parsing.String
import Parsing.String.Basic as Parsing.String.Basic
import Control.Alt ((<|>))

pTrueShorthand :: Parser String Boolean
pTrueShorthand = do
  _ <- Parsing.String.Basic.oneOf [ 't', 'y', '1' ]
  Parsing.String.eof <?> "end of string"
  pure true

pFalseShorthand :: Parser String Boolean
pFalseShorthand = do
  _ <- Parsing.String.Basic.oneOf [ 'f', 'n', '0' ]
  Parsing.String.eof <?> "end of string"
  pure false

pTrue :: Parser String Boolean
pTrue = do
  _ <- Parsing.String.string "on"
    <|> Parsing.String.string "true"
    <|> Parsing.String.string "yes"
  Parsing.String.eof <?> "end of string"
  pure true

pFalse :: Parser String Boolean
pFalse = do
  _ <- Parsing.String.string "off"
    <|> Parsing.String.string "false"
    <|> Parsing.String.string "no"
  Parsing.String.eof <?> "end of string"
  pure false

-- | Parse a string as a possible boolean.
parse :: String -> Either String Boolean
parse = lmap Parsing.parseErrorMessage <<< flip Parsing.runParser parser

-- | INTERNAL
-- |
-- | A parser for Flatfile specific boolean.
parser :: Parser String Boolean
parser = pTrue
  <|> pFalse
  <|> pTrueShorthand
  <|> pFalseShorthand
