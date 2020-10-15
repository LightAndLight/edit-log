module Parser
  ( Parser
  , ParseError(..)
  , runParser
  , expr
  )
where

import Control.Applicative ((<|>), many, optional, some)
import Text.Parser.Combinators (eof)
import Text.Parser.Char (CharParsing, char, digit, string, spaces)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)

import Syntax (Expr(..), BinOp(..), UnOp(..))

type Parser = ReadP

data ParseError = ParseError
  deriving Show

runParser :: Parser a -> String -> Either ParseError a
runParser p input =
  case readP_to_S (p <* eof) input of
    [] -> Left ParseError
    (a, _) : _ -> Right a

expr :: CharParsing m => m Expr
expr = orOp
  where
    token p = p <* spaces

    orOp = foldl (BinOp Or) <$> andOp <*> many (token (string "or") *> andOp)

    andOp = foldl (BinOp And) <$> eqOp <*> many (token (string "and") *> eqOp)

    eqOp = foldl (BinOp Eq) <$> addSub <*> many (token (string "==") *> addSub)

    addSub =
      foldl (\left (op, right) -> BinOp op left right) <$>
      mulDiv <*>
      many ((,) <$> token (Add <$ char '+' <|> Sub <$ char '-') <*> mulDiv)

    mulDiv =
      foldl (\left (op, right) -> BinOp op left right) <$>
      unOp <*>
      many ((,) <$> token (Mul <$ char '*' <|> Div <$ char '/') <*> unOp)

    unOp =
      (\mOp ex -> maybe ex (`UnOp` ex) mOp) <$>
      optional (Neg <$ char '-' <|> Not <$ token (string "not")) <*>
      atom

    atom =
      token $
      bool <|>
      int <|>
      hole

    bool =
      Bool True <$ string "true" <|>
      Bool False <$ string "false"

    int =
      Int . read <$>
      some digit

    hole = EHole <$ char '?'
