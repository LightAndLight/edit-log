module Parser
  ( Parser
  , ParseError(..)
  , runParser
  , expr
  , ident
  , simpleStatement
  , list
  )
where

import Control.Applicative ((<|>), many, optional, some)
import Text.Parser.Combinators (between, eof, sepBy)
import Text.Parser.Char (CharParsing, alphaNum, char, digit, lower, string, spaces)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)

import Syntax (Block(..), Expr(..), Ident(..), List(..), Statement(..), BinOp(..), UnOp(..))

type Parser = ReadP

data ParseError = ParseError
  deriving Show

runParser :: Parser a -> String -> Either ParseError a
runParser p input =
  case readP_to_S (p <* eof) input of
    [] -> Left ParseError
    (a, _) : _ -> Right a

token :: CharParsing m => m a -> m a
token p = p <* spaces

expr :: CharParsing m => m Expr
expr = orOp
  where
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
      EIdent <$> identifier <|>
      hole

    bool =
      Bool True <$ string "true" <|>
      Bool False <$ string "false"

    int =
      Int . read <$>
      some digit

    hole = EHole <$ char '?'

identifier :: CharParsing m => m String
identifier = (:) <$> lower <*> many alphaNum

ident :: CharParsing m => m Ident
ident =
  Ident <$> identifier <|>
  IHole <$ char '?'

simpleStatement :: CharParsing m => m Statement
simpleStatement =
  forSt <|>
  ifSt <|>
  printSt <|>
  defSt <|>
  SHole <$ token (char '?')
  where
    printSt =
      Print <$ token (string "print") <* token (char ':') <*>
      expr

    forSt =
      (\i e -> For i e . Block . pure) <$ token (string "for") <*>
      token ident <* token (string "in") <*>
      expr <* token (char ':') <*>
      simpleStatement

    ifSt =
      (\cond then_ mElse_ ->
         maybe (IfThen cond . Block $ pure then_) (IfThenElse cond (Block $ pure then_) . Block . pure) mElse_
      ) <$ token (string "if") <*>
      expr <* token (char ':') <*>
      simpleStatement <*>
      optional (token (string "else") *> token (char ':') *> simpleStatement)

    defSt =
      (\n args -> Def n args . Block . pure) <$ token (string "def") <*>
      token ident <*>
      between
        (token $ char '(')
        (token $ char ')')
        (list ident) <* token (char ':') <*>
      simpleStatement

list :: CharParsing m => m a -> m (List a)
list ma = (fmap List $ ma `sepBy` token (char ','))
