{-# language OverloadedStrings #-}
module Attrs where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

newtype Attrs = Attrs { unAttrs :: Map Text Text }

(=:) :: Text -> Text -> Attrs
(=:) k v = Attrs $ Map.singleton k v

infix 7 =:

instance Semigroup Attrs where
  Attrs as <> Attrs bs =
    Attrs $
    Map.unionWithKey
      (\k a b ->
         if k == "class"
         then
           if a == ""
           then b
           else
             if b == ""
             then a
             else a <> " " <> b
         else b
      )
      as
      bs

instance Monoid Attrs where; mempty = Attrs mempty
