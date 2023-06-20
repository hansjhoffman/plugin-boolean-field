module Main
  ( isRight_
  , parse_
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either, isRight)
import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators (choice, try, (<?>))
import Parsing.String (eof, string)
import Parsing.String.Basic (oneOf)

-- | INTERNAL
-- |
-- | A parser for Flatfile specific booleans.
parser :: Parser String Boolean
parser =
  try
    ( choice
        [ pTrueLonghand
        , pFalseLonghand
        , pTrueShorthand
        , pFalseShorthand
        ]
    ) <?> "one of [ 't', 'f', 'y', 'n', '1', '0', 'on', 'off', 'yes', 'no', 'true', 'false' ]"
  where
  pTrueShorthand :: Parser String Boolean
  pTrueShorthand =
    oneOf [ 't', 'y', '1' ] *> eof *> pure true

  pFalseShorthand :: Parser String Boolean
  pFalseShorthand =
    oneOf [ 'f', 'n', '0' ] *> eof *> pure false

  pTrueLonghand :: Parser String Boolean
  pTrueLonghand =
    choice
      [ string "on"
      , string "true"
      , string "yes"
      ]
      *> eof
      *> pure true

  pFalseLonghand :: Parser String Boolean
  pFalseLonghand =
    choice
      [ string "off"
      , string "false"
      , string "no"
      ]
      *> eof
      *> pure false

-- | Parse a string as a possible boolean.
parse_ :: String -> Either String Boolean
parse_ = lmap Parsing.parseErrorMessage
  <<< flip Parsing.runParser parser

-- | How we get around the fact that the bundler (via dead code elimination) does not export
-- | a way to operate on the 'Either' JS class instead of TS discriminated union types.
isRight_ :: forall a b. Either a b -> Boolean
isRight_ = isRight
