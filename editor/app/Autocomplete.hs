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
  [ ("not ?", UnOp Not EHole)
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
  [ ("for in", For IHole EHole $ Block [SHole])
  , ("if", IfThen EHole $ Block [SHole])
  , ("if else", IfThenElse EHole (Block [SHole]) (Block [SHole]))
  , ("print", Print EHole)
  , ("def", Def IHole [] $ Block [SHole])
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
          prefixWeight + suffixWeight / (1 + fromIntegral (Text.length after))
        Infix before after ->
          prefixWeight / (1 + fromIntegral (Text.length before)) +
          suffixWeight / (1 + fromIntegral (Text.length after))
  where
    prefixWeight = 0.6
    suffixWeight = 0.4
