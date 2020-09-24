{-# language OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import Data.Functor.Identity (Identity(..))
import qualified Text.Blaze.Html5.Attributes as Attr
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as Html

import qualified Diff
import qualified Log
import qualified Render
import Path (Path(..), Level(..))
import qualified Store
import Syntax (Expr(..), BinOp(..))
import qualified Versioned
import Versioned.Pure (Versioned, newVersioned, runVersionedT)

main :: IO ()
main = do
  let
    initial :: Expr
    initial = BinOp Add (Int 1) (Int 2)

    v :: Versioned Expr
    v = newVersioned initial

    Identity ((diff, diffHtml, es), _v') = runVersionedT v $ do
      Versioned.replace (Cons BinOp_Left Nil) (Int 22)
      Versioned.replace (Cons BinOp_Right Nil) (BinOp Mul (Int 3) (Int 4))
      es <- fmap snd <$> Log.getEntries
      diff <- Diff.toDiff es
      diffHtml <- Render.renderExprWithDiff initial diff
      pure (diff, diffHtml, es)
  print es
  print diff
  LazyBS.writeFile "output.css" $
    LazyChar8.unlines
    [ ".diff-added {"
    , "  background-color: green;"
    , "}"
    , ""
    , ".diff-removed {"
    , "  background-color: red;"
    , "}"
    , ""
    , ".syntax-line {"
    , "  display: flex;"
    , "  flex-direction: row;"
    , "  align-items: center;"
    , "}"
    , ""
    , ".expr.diff {"
    , "  display: flex;"
    , "  flex-direction: row;"
    , "}"
    , ""
    , ".syntax-leaf {"
    , "  padding: 1em;"
    , "}"
    ]
  LazyBS.writeFile "output.html" . Blaze.renderHtml $ do
    Html.docTypeHtml $ do
      Html.head $ do
        Html.meta ! Attr.charset "UTF-8"
        Html.link ! Attr.rel "stylesheet" ! Attr.href "output.css"
        Html.title "renderdiff"
      Html.body $
        diffHtml
