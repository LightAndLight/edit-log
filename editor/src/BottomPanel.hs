{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
module BottomPanel
  ( BottomPanel(..), bpNextError, bpPrevError, bpSetFocus
  , renderBottomPanel
  )
where

import Control.Lens.TH (makeLenses)
import Control.Monad.Fix (MonadFix)
import qualified Data.Dependent.Map as DMap
import Data.Bifunctor.Flip (Flip(..))
import Data.Constraint.Extras (has)
import Data.Dependent.Sum (DSum(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Type.Equality ((:~:)(..))
import Reflex
import Reflex.Dom (DomBuilder)
import qualified Reflex.Dom as Dom

import Check (CheckError(..))
import NodeType (KnownNodeType)
import Path (Path(..))
import qualified Path as Path
import Path.Trie (Trie)
import qualified Path.Trie as Trie

import Focus (Focus(..))

errorIcon :: DomBuilder t m => m ()
errorIcon =
  Dom.elAttr "i" [("class", "ri-close-circle-line ri-fw"), ("style", "display: inline-block; font-size: 1.2em; color: red;")] $
  pure ()

renderHeader :: DomBuilder t m => m ()
renderHeader =
  Dom.elAttr "div" [("id", "bottom-panel-header")] $ do
    Dom.elAttr "span" [("class", "bottom-panel-header-item bottom-panel-header-item-active")] $ Dom.text "Problems"

data BottomPanel t a
  = BottomPanel
  { _bpNextError :: Event t ()
  , _bpPrevError :: Event t ()
  , _bpSetFocus :: Event t (Focus a)
  }
makeLenses ''BottomPanel

prettyError :: CheckError a -> Text
prettyError err =
  case err of
    NotInScope n -> "'" <> Text.pack n <> "' is not in scope"

renderError ::
  (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, KnownNodeType b) =>
  Path a b ->
  Dynamic t Bool ->
  CheckError b ->
  m (Event t (Focus a))
renderError path dFocused err = do
  rec
    let
      dAttrs =
        ((\focused hovered ->
           [( "class"
            , "bottom-panel-error" <>
              (if focused then " bottom-panel-error-focused" else "") <>
              (if hovered then " bottom-panel-error-hovered" else "")
            )
           ]
         ) <$>
         dFocused <*>
         dHovered
        )

    (el, _) <-
      Dom.elDynAttr' "div" dAttrs $ do
        Dom.elAttr "div" [("style", "display: flex; flex-direction: row; align-items: center;")] $ do
          errorIcon
          Dom.elAttr "span" [("style", "display: inline-block; margin-left: 0.25em;")] $ do
            Dom.text $ prettyError err

    let
      eMouseEnter = Dom.domEvent Dom.Mouseenter el
      eMouseLeave = Dom.domEvent Dom.Mouseleave el
      eMouseDown = Dom.domEvent Dom.Mousedown el

    dHovered <- holdDyn False $ leftmost [True <$ eMouseEnter, False <$ eMouseLeave]

  let eFocusOnError = Focus path <$ eMouseDown

  pure eFocusOnError

renderErrors ::
  forall t m a b.
  (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, KnownNodeType b) =>
  Dynamic t (Focus b) ->
  Path a b ->
  Trie b CheckError ->
  m (Event t (Focus a))
renderErrors dFocus path trie = do
  dFocused <- holdUniqDyn $ (\case; Focus Nil -> True; _ -> False) <$> dFocus

  eFocusOnError <-
    case Trie.current trie of
      Nothing -> pure never
      Just err -> renderError path dFocused err

  eFocusOnErrors <-
    leftmost <$>
    traverse
      (\(l :=> Flip trie') ->
         has @KnownNodeType l $
         renderErrors
           ((\case
                 Focus (Cons l' newFocus) | Just Refl <- Path.eqLevel l l' -> Focus newFocus
                 _ -> NoFocus
             ) <$> dFocus
           )
           (Path.snoc path l)
           trie'
      )
      (DMap.toAscList $ Trie.levels trie)

  pure $ leftmost [eFocusOnError, eFocusOnErrors]

renderBottomPanel ::
  forall t m a.
  (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, KnownNodeType a) =>
  Dynamic t (Focus a) ->
  Dynamic t (Trie a CheckError) ->
  m (BottomPanel t a)
renderBottomPanel dFocus dErrors =
  Dom.elAttr "div" [("id", "bottom-panel")] $ do
    renderHeader
    Dom.elAttr "div" [("id", "bottom-panel-body")] $ do
      eSetFocus <-
        switchDyn <$>
        Dom.widgetHold
          (sample (current dErrors) >>= renderErrors dFocus Nil)
          (renderErrors dFocus Nil <$> updated dErrors)

      pure $ BottomPanel { _bpNextError = never, _bpPrevError = never, _bpSetFocus = eSetFocus }
