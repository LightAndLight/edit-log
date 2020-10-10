{-# language GADTs #-}
{-# language OverloadedStrings #-}
module Main where

import Control.Monad.Fix (MonadFix)
import Data.Functor.Identity (Identity(..))
import Data.Foldable (traverse_)
import Reflex
import Reflex.Dom

import Hash (Hash)
import Log (Entry, Time)
import Node (Node(..))
import Syntax (Block(..), Statement(..))
import Session (Session, newSession)
import Session.Pure (runSessionT)
import qualified Store
import qualified Versioned
import Versioned.Pure (Versioned, runVersionedT, newVersioned)

renderNode :: DomBuilder t m => Versioned a -> Hash b -> m ()
renderNode versioned h = do
  let Identity (mNode, _) = runVersionedT versioned $ Store.lookupNode h
  case mNode of
    Nothing -> text "error: missing node"
    Just node ->
      case node of
        NFor ident val body -> error "TODO: for" ident val body
        NIfThen cond then_ -> error "TODO: ifThen" cond then_
        NIfThenElse cond then_ else_ -> error "TODO: ifThenElse" cond then_ else_
        NPrint val -> error "TODO: print" val
        NBool b -> error "TODO: bool" b
        NInt n -> error "TODO: int" n
        NBinOp op left right -> error "TODO: binOp" op left right
        NUnOp op val -> error "TODO: unOp" op val
        NBlock sts -> traverse_ (renderNode versioned) sts
        NSHole -> text "_"
        NEHole -> text "_"

program ::
  (Reflex t, MonadHold t m, PostBuild t m, DomBuilder t m, MonadFix m) =>
  m ()
program = do
  let
    initialVersioned :: Versioned Block
    initialVersioned = newVersioned $ Block [SHole]

    initialSession :: Session (Time, Entry Block)
    initialSession = newSession

  (dVersioned, dSession) <-
    splitDynPure <$>
    foldDyn
      (\action@() (versioned, session) ->
          let
            Identity ((), versioned', session') = runSessionT versioned session $ pure ()
          in
            (versioned', session')
      )
      (initialVersioned, initialSession)
      never

  dyn_ $
    (\versioned -> do
       let Identity (rooth, _) = runVersionedT versioned Versioned.getRoot
       renderNode versioned rooth
    ) <$>
    dVersioned

main :: IO ()
main = do
  mainWidgetWithHead
    (do
       elAttr "meta" ("charset" =: "UTF-8") $ pure ()
       el "title" $ text "Editor"
       elAttr "link"
         ("rel" =: "stylesheet" <> "href" =: "https://fonts.googleapis.com/css2?family=Source+Code+Pro&display=swap")
         (pure ())
       el "style" $
         text "html { font-family: 'Source Code Pro', monospace; }"
    )
    program
