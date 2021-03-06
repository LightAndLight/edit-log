module Parser
  ( Parser
  , ParseError(..)
  , runParser
  , expr
  , ident
  , simpleStatement
  , exprs
  , args
  , params
  )
where

import Control.Applicative ((<|>), many, optional, some)
import Text.Parser.Combinators (between, eof, sepBy)
import Text.Parser.Char (CharParsing, alphaNum, char, digit, lower, string, spaces)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)

import Syntax (Block(..), Expr(..), Ident(..), Statement(..), BinOp(..), UnOp(..), Args(..), Params(..), Exprs(..))

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
      call

    call =
      (\e -> maybe e (Call e)) <$>
      atom <*>
      optional (parens args)

    atom =
      token $
      bool <|>
      int <|>
      EIdent <$> identifier <|>
      elist <|>
      hole

    elist =
      List <$>
      between (token $ char '[') (token $ char ']') exprs

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
  returnSt <|>
  defSt <|>
  SHole <$ token (char '?')
  where
    printSt =
      Print <$ token (string "print") <* token (char ':') <*>
      expr

    returnSt =
      Return <$ token (string "return") <* token (char ':') <*>
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
      (\n as -> Def n as . Block . pure) <$ token (string "def") <*>
      token ident <*>
      parens params <*>
      simpleStatement

parens :: CharParsing m => m a -> m a
parens = between (token $ char '(') (token $ char ')')

args :: CharParsing m => m Args
args = Args <$> list expr

params :: CharParsing m => m Params
params = Params <$> list ident

exprs :: CharParsing m => m Exprs
exprs = Exprs <$> list expr

list :: CharParsing m => m a -> m [a]
list ma = (ma `sepBy` token (char ','))
