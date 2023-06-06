module Main (parse) where

import Prelude

import Control.Alt ((<|>))
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.String as Str
import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators ((<?>))
import Parsing.Combinators as Parsing.Combinators
import Parsing.String as Parsing.String
import Parsing.String.Basic as Parsing.String.Basic

-- | INTERNAL
pTrueShorthand :: Parser String Boolean
pTrueShorthand = do
  _ <- Parsing.String.Basic.oneOf [ 't', 'y', '1' ]
  Parsing.String.eof
  pure true

-- | INTERNAL
pFalseShorthand :: Parser String Boolean
pFalseShorthand = do
  _ <- Parsing.String.Basic.oneOf [ 'f', 'n', '0' ]
  Parsing.String.eof
  pure false

-- | INTERNAL
pTrueLonghand :: Parser String Boolean
pTrueLonghand = do
  _ <- Parsing.String.string "on"
    <|> Parsing.String.string "true"
    <|> Parsing.String.string "yes"
  Parsing.String.eof
  pure true

-- | INTERNAL
pFalseLonghand :: Parser String Boolean
pFalseLonghand = do
  _ <- Parsing.String.string "off"
    <|> Parsing.String.string "false"
    <|> Parsing.String.string "no"
  Parsing.String.eof
  pure false

-- | INTERNAL
-- |
-- | A parser for Flatfile specific booleans.
parser :: Parser String Boolean
parser = do
  Parsing.Combinators.try
    ( pTrueLonghand
        <|> pFalseLonghand
        <|> pTrueShorthand
        <|> pFalseShorthand
    ) <?> "one of [ 't', 'y', '1', 'f', 'n', '0', 'on', 'true', 'yes', 'off', 'false', 'no' ]"

-- | Parse a string as a possible boolean.
parse :: String -> Either String Boolean
parse = lmap Parsing.parseErrorMessage
  <<< flip Parsing.runParser parser
  <<< (Str.toLower <<< Str.trim)
