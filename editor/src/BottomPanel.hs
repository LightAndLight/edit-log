{-# language LambdaCase #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
module BottomPanel
  ( BottomPanel(..), bpNextError, bpPrevError
  , renderBottomPanel
  )
where

import Control.Lens.TH (makeLenses)
import Control.Monad.Fix (MonadFix)
import Data.Functor ((<&>))
import Data.Some (Some(..))
import qualified Data.Text as Text
import Reflex
import Reflex.Dom (DomBuilder)
import qualified Reflex.Dom as Dom

import Check (CheckError)
import Path.Trie (Trie)
import qualified Path.Trie as Trie

import Focus (Focus(..))

data BottomPanel t
  = BottomPanel
  { _bpNextError :: Event t ()
  , _bpPrevError :: Event t ()
  }
makeLenses ''BottomPanel

renderBottomPanel ::
  forall t m a.
  (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) =>
  Dynamic t (Focus a) ->
  Dynamic t (Trie a CheckError) ->
  m (BottomPanel t)
renderBottomPanel dFocus dErrors =
  Dom.elAttr "div" [("id", "bottom-panel"), ("style", "border-top: 1px solid black")] $ do
    let
      dmError :: Dynamic t (Maybe (Some CheckError))
      dmError =
        (\focus errors ->
           case focus of
             NoFocus -> Nothing
             Focus path -> Some <$> Trie.lookup path errors
        ) <$>
        dFocus <*>
        dErrors
    dmdError :: Dynamic t (Maybe (Dynamic t (Some CheckError))) <- maybeDyn dmError
    Dom.dyn_ $
      dmdError <&>
      \case
        Nothing -> pure ()
        Just dError -> Dom.dyn_ $ Dom.text . Text.pack . show <$> dError

    pure $ BottomPanel { _bpNextError = never, _bpPrevError = never }
