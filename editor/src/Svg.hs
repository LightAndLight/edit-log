{-# language OverloadedStrings #-}
module Svg where

import Data.Map (Map)
import Data.Text (Text)
import Reflex.Dom (DomBuilder, PostBuild)
import qualified Reflex.Dom as Dom

svgEl :: (DomBuilder t m, PostBuild t m) => Text -> m a -> m a
svgEl t m = do
  (_, a) <- Dom.elDynAttrNS' (Just "http://www.w3.org/2000/svg") t (pure mempty) m
  pure a

svgElAttr :: (DomBuilder t m, PostBuild t m) => Text -> Map Text Text -> m a -> m a
svgElAttr t attrs m = do
  (_, a) <- Dom.elDynAttrNS' (Just "http://www.w3.org/2000/svg") t (pure attrs) m
  pure a
