module Parser
  ( Parser
  , ParseError(..)
  , runParser
  , expr
  )
where

import Control.Applicative ((<|>), some)
import Text.Parser.Combinators (eof)
import Text.Parser.Char (CharParsing, char, digit, string)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)

import Syntax (Expr(..))

type Parser = ReadP

data ParseError = ParseError
  deriving Show

runParser :: Parser a -> String -> Either ParseError a
runParser p input =
  case readP_to_S (p <* eof) input of
    [] -> Left ParseError
    (a, _) : _ -> Right a

expr :: CharParsing m => m Expr
expr =
  bool <|>
  int <|>
  hole
  where
    bool =
      Bool True <$ string "true" <|>
      Bool False <$ string "false"

    int =
      (\f -> Int . f . read) <$>
      (negate <$ char '-' <|> pure id) <*>
      some digit

    hole = EHole <$ char '?'
