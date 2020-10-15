{-# language OverloadedStrings #-}
module Autocomplete
  ( similarity
  , baseExprCompletions
  , baseStatementCompletions
  )
where

import Data.Text (Text)
import qualified Data.Text as Text

import Syntax (Block(..), Expr(..), Ident(..), Statement(..), UnOp(..), BinOp(..))

baseExprCompletions :: [(Text, Expr)]
baseExprCompletions =
  [ ("true", Bool True)
  , ("false", Bool False)
  , ("not ?", UnOp Not EHole)
  , ("-?", UnOp Neg EHole)
  , ("? + ?", BinOp Add EHole EHole)
  , ("? - ?", BinOp Sub EHole EHole)
  , ("? * ?", BinOp Mul EHole EHole)
  , ("? / ?", BinOp Div EHole EHole)
  , ("? == ?", BinOp Eq EHole EHole)
  , ("? and ?", BinOp And EHole EHole)
  , ("? or ?", BinOp Or EHole EHole)
  ]

baseStatementCompletions :: [(Text, Statement)]
baseStatementCompletions =
  [ ("for in", For IHole EHole $ Block (pure SHole))
  , ("if", IfThen EHole $ Block (pure SHole))
  , ("if else", IfThenElse EHole (Block $ pure SHole) (Block $ pure SHole))
  , ("print", Print EHole)
  , ("def", Def IHole [] $ Block (pure SHole))
  ]

data Found
  = Equal
  | Prefix Text
  | Infix Text Text

findIn :: Text -> Text -> Maybe Found
findIn needle haystack
  | Text.null needle = Just $ Prefix haystack
  | otherwise =
      case Text.splitOn needle haystack of
        [_] -> Nothing -- not found
        ["", ""] -> Just Equal
        "" : rest -> Just . Prefix $ Text.concat rest
        prefix : rest -> Just . Infix prefix $ Text.concat rest
        [] -> error "impossible"

similarity :: Text -> Text -> Float
similarity needle haystack =
  case findIn needle haystack of
    Nothing -> 0
    Just found ->
      case found of
        Equal -> 1
        Prefix after ->
          prefixWeight + suffixScore after
        Infix before after ->
          prefixScore before + suffixScore after
  where
    prefixWeight = 0.6
    suffixWeight = 0.4

    prefixScore before = prefixWeight / exp (0.2 * fromIntegral (Text.length before))
    suffixScore after = suffixWeight / exp (0.2 * fromIntegral (Text.length after))
