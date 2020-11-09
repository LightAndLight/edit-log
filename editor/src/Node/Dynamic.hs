{-# language GADTs, KindSignatures #-}
{-# language LambdaCase #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# options_ghc -fno-warn-overlapping-patterns #-}
module Node.Dynamic
  ( NodeTag(..), fanNode
  , NodeD(..), nodeDyn
  )
where

import Control.Lens.Getter (view)
import Control.Lens.Tuple (_1, _2, _3)
import Control.Monad.Fix (MonadFix)
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare (GEq(..), GCompare(..), GOrdering(..))
import Data.List.NonEmpty (NonEmpty)
import Data.Type.Equality ((:~:)(..))
import Reflex
import qualified Reflex.Dom as Dom

import Hash (Hash)
import Node (Node(..))
import Syntax (Statement, Expr, Block, Ident, UnOp, BinOp, Params, Args, Exprs)

data NodeD t :: * -> * where
  NForD ::
    Hash Ident ->
    Hash Expr ->
    Hash Block ->
    Dynamic t (Hash Ident) ->
    Dynamic t (Hash Expr) ->
    Dynamic t (Hash Block) ->
    NodeD t Statement
  NIfThenD ::
    Hash Expr ->
    Hash Block ->
    Dynamic t (Hash Expr) ->
    Dynamic t (Hash Block) ->
    NodeD t Statement
  NIfThenElseD ::
    Hash Expr ->
    Hash Block ->
    Hash Block ->
    Dynamic t (Hash Expr) ->
    Dynamic t (Hash Block) ->
    Dynamic t (Hash Block) ->
    NodeD t Statement
  NPrintD ::
    Hash Expr ->
    Dynamic t (Hash Expr) ->
    NodeD t Statement
  NReturnD ::
    Hash Expr ->
    Dynamic t (Hash Expr) ->
    NodeD t Statement
  NDefD ::
    Hash Ident ->
    Hash Params ->
    Hash Block ->
    Dynamic t (Hash Ident) ->
    Dynamic t (Hash Params) ->
    Dynamic t (Hash Block) ->
    NodeD t Statement

  NBoolD ::
    Bool ->
    Dynamic t Bool ->
    NodeD t Expr
  NIntD ::
    Int ->
    Dynamic t Int ->
    NodeD t Expr
  NBinOpD ::
    BinOp ->
    Hash Expr ->
    Hash Expr ->
    Dynamic t BinOp ->
    Dynamic t (Hash Expr) ->
    Dynamic t (Hash Expr) ->
    NodeD t Expr
  NUnOpD ::
    UnOp ->
    Hash Expr ->
    Dynamic t UnOp ->
    Dynamic t (Hash Expr) ->
    NodeD t Expr
  NCallD ::
    Hash Expr ->
    Hash Args ->
    Dynamic t (Hash Expr) ->
    Dynamic t (Hash Args) ->
    NodeD t Expr
  NListD ::
    Hash Exprs ->
    Dynamic t (Hash Exprs) ->
    NodeD t Expr
  NEIdentD ::
    Dynamic t String ->
    NodeD t Expr

  NBlockD ::
    NonEmpty (Hash Statement) ->
    Dynamic t (NonEmpty (Hash Statement)) ->
    NodeD t Block

  NIdentD ::
    Dynamic t String ->
    NodeD t Ident

  NExprsD ::
    [Hash Expr] ->
    Dynamic t [Hash Expr] ->
    NodeD t Exprs
  NArgsD ::
    [Hash Expr] ->
    Dynamic t [Hash Expr] ->
    NodeD t Args
  NParamsD ::
    [Hash Ident] ->
    Dynamic t [Hash Ident] ->
    NodeD t Params

  NSHoleD :: NodeD t Statement
  NEHoleD :: NodeD t Expr
  NIHoleD :: NodeD t Ident

data NodeTag :: * -> * -> * where
  NTagFor :: NodeTag Statement (Hash Ident, Hash Expr, Hash Block)
  NTagIfThen :: NodeTag Statement (Hash Expr, Hash Block)
  NTagIfThenElse :: NodeTag Statement (Hash Expr, Hash Block, Hash Block)
  NTagPrint :: NodeTag Statement (Hash Expr)
  NTagReturn :: NodeTag Statement (Hash Expr)
  NTagDef :: NodeTag Statement (Hash Ident, Hash Params, Hash Block)

  NTagBool :: NodeTag Expr Bool
  NTagInt :: NodeTag Expr Int
  NTagBinOp :: NodeTag Expr (BinOp, Hash Expr, Hash Expr)
  NTagUnOp :: NodeTag Expr (UnOp, Hash Expr)
  NTagCall :: NodeTag Expr (Hash Expr, Hash Args)
  NTagList :: NodeTag Expr (Hash Exprs)
  NTagEIdent :: NodeTag Expr String

  NTagBlock :: NodeTag Block (NonEmpty (Hash Statement))

  NTagIdent :: NodeTag Ident String

  NTagExprs :: NodeTag Exprs [Hash Expr]
  NTagArgs :: NodeTag Args [Hash Expr]
  NTagParams :: NodeTag Params [Hash Ident]

  NTagSHole :: NodeTag Statement ()
  NTagEHole :: NodeTag Expr ()
  NTagIHole :: NodeTag Ident ()
instance GEq (NodeTag a) where
  geq NTagFor NTagFor = Just Refl
  geq NTagFor _ = Nothing
  geq _ NTagFor = Nothing

  geq NTagIfThen NTagIfThen = Just Refl
  geq NTagIfThen _ = Nothing
  geq _ NTagIfThen = Nothing

  geq NTagIfThenElse NTagIfThenElse = Just Refl
  geq NTagIfThenElse _ = Nothing
  geq _ NTagIfThenElse = Nothing

  geq NTagPrint NTagPrint = Just Refl
  geq NTagPrint _ = Nothing
  geq _ NTagPrint = Nothing

  geq NTagReturn NTagReturn = Just Refl
  geq NTagReturn _ = Nothing
  geq _ NTagReturn = Nothing

  geq NTagDef NTagDef = Just Refl
  geq NTagDef _ = Nothing
  geq _ NTagDef = Nothing

  geq NTagBool NTagBool = Just Refl
  geq NTagBool _ = Nothing
  geq _ NTagBool = Nothing

  geq NTagInt NTagInt = Just Refl
  geq NTagInt _ = Nothing
  geq _ NTagInt = Nothing

  geq NTagBinOp NTagBinOp = Just Refl
  geq NTagBinOp _ = Nothing
  geq _ NTagBinOp = Nothing

  geq NTagUnOp NTagUnOp = Just Refl
  geq NTagUnOp _ = Nothing
  geq _ NTagUnOp = Nothing

  geq NTagCall NTagCall = Just Refl
  geq NTagCall _ = Nothing
  geq _ NTagCall = Nothing

  geq NTagList NTagList = Just Refl
  geq NTagList _ = Nothing
  geq _ NTagList = Nothing

  geq NTagEIdent NTagEIdent = Just Refl
  geq NTagEIdent _ = Nothing
  geq _ NTagEIdent = Nothing

  geq NTagBlock NTagBlock = Just Refl
  geq NTagBlock _ = Nothing
  geq _ NTagBlock = Nothing

  geq NTagIdent NTagIdent = Just Refl
  geq NTagIdent _ = Nothing
  geq _ NTagIdent = Nothing

  geq NTagExprs NTagExprs = Just Refl
  geq NTagExprs _ = Nothing
  geq _ NTagExprs = Nothing

  geq NTagParams NTagParams = Just Refl
  geq NTagParams _ = Nothing
  geq _ NTagParams = Nothing

  geq NTagArgs NTagArgs = Just Refl
  geq NTagArgs _ = Nothing
  geq _ NTagArgs = Nothing

  geq NTagSHole NTagSHole = Just Refl
  geq NTagSHole _ = Nothing
  geq _ NTagSHole = Nothing

  geq NTagEHole NTagEHole = Just Refl
  geq NTagEHole _ = Nothing
  geq _ NTagEHole = Nothing

  geq NTagIHole NTagIHole = Just Refl
  geq NTagIHole _ = Nothing
  geq _ NTagIHole = Nothing
instance GCompare (NodeTag a) where
  gcompare NTagFor NTagFor = GEQ
  gcompare NTagFor _ = GLT
  gcompare _ NTagFor = GGT

  gcompare NTagIfThen NTagIfThen = GEQ
  gcompare NTagIfThen _ = GLT
  gcompare _ NTagIfThen = GGT

  gcompare NTagIfThenElse NTagIfThenElse = GEQ
  gcompare NTagIfThenElse _ = GLT
  gcompare _ NTagIfThenElse = GGT

  gcompare NTagPrint NTagPrint = GEQ
  gcompare NTagPrint _ = GLT
  gcompare _ NTagPrint = GGT

  gcompare NTagReturn NTagReturn = GEQ
  gcompare NTagReturn _ = GLT
  gcompare _ NTagReturn = GGT

  gcompare NTagDef NTagDef = GEQ
  gcompare NTagDef _ = GLT
  gcompare _ NTagDef = GGT

  gcompare NTagBool NTagBool = GEQ
  gcompare NTagBool _ = GLT
  gcompare _ NTagBool = GGT

  gcompare NTagInt NTagInt = GEQ
  gcompare NTagInt _ = GLT
  gcompare _ NTagInt = GGT

  gcompare NTagBinOp NTagBinOp = GEQ
  gcompare NTagBinOp _ = GLT
  gcompare _ NTagBinOp = GGT

  gcompare NTagUnOp NTagUnOp = GEQ
  gcompare NTagUnOp _ = GLT
  gcompare _ NTagUnOp = GGT

  gcompare NTagCall NTagCall = GEQ
  gcompare NTagCall _ = GLT
  gcompare _ NTagCall = GGT

  gcompare NTagList NTagList = GEQ
  gcompare NTagList _ = GLT
  gcompare _ NTagList = GGT

  gcompare NTagEIdent NTagEIdent = GEQ
  gcompare NTagEIdent _ = GLT
  gcompare _ NTagEIdent = GGT

  gcompare NTagBlock NTagBlock = GEQ
  gcompare NTagBlock _ = GLT
  gcompare _ NTagBlock = GGT

  gcompare NTagIdent NTagIdent = GEQ
  gcompare NTagIdent _ = GLT
  gcompare _ NTagIdent = GGT

  gcompare NTagExprs NTagExprs = GEQ
  gcompare NTagExprs _ = GLT
  gcompare _ NTagExprs = GGT

  gcompare NTagParams NTagParams = GEQ
  gcompare NTagParams _ = GLT
  gcompare _ NTagParams = GGT

  gcompare NTagArgs NTagArgs = GEQ
  gcompare NTagArgs _ = GLT
  gcompare _ NTagArgs = GGT

  gcompare NTagSHole NTagSHole = GEQ
  gcompare NTagSHole _ = GLT
  gcompare _ NTagSHole = GGT

  gcompare NTagEHole NTagEHole = GEQ
  gcompare NTagEHole _ = GLT
  gcompare _ NTagEHole = GGT

  gcompare NTagIHole NTagIHole = GEQ
  gcompare NTagIHole _ = GLT
  gcompare _ NTagIHole = GGT

fanNode :: Reflex t => Event t (Node a) -> EventSelector t (NodeTag a)
fanNode =
  fan .
  fmapCheap
    (\case
      NFor i e b -> DMap.singleton NTagFor $ pure (i, e, b)
      NIfThen c t -> DMap.singleton NTagIfThen $ pure (c, t)
      NIfThenElse c t e -> DMap.singleton NTagIfThenElse $ pure (c, t, e)
      NPrint v -> DMap.singleton NTagPrint $ pure v
      NReturn v -> DMap.singleton NTagReturn $ pure v
      NDef n p b -> DMap.singleton NTagDef $ pure (n, p, b)
      NBool b -> DMap.singleton NTagBool $ pure b
      NInt n -> DMap.singleton NTagInt $ pure n
      NBinOp op l r -> DMap.singleton NTagBinOp $ pure (op, l, r)
      NUnOp op v -> DMap.singleton NTagUnOp $ pure (op, v)
      NCall f x -> DMap.singleton NTagCall $ pure (f, x)
      NList xs -> DMap.singleton NTagList $ pure xs
      NEIdent i -> DMap.singleton NTagEIdent $ pure i
      NBlock sts -> DMap.singleton NTagBlock $ pure sts
      NIdent i -> DMap.singleton NTagIdent $ pure i
      NExprs es -> DMap.singleton NTagExprs $ pure es
      NArgs es -> DMap.singleton NTagArgs $ pure es
      NParams es -> DMap.singleton NTagParams $ pure es
      NSHole -> DMap.singleton NTagSHole $ pure ()
      NEHole -> DMap.singleton NTagEHole $ pure ()
      NIHole -> DMap.singleton NTagIHole $ pure ()
    )

uniqDyn :: (Reflex t, MonadHold t m, Eq a, MonadFix m) => a -> Event t a -> m (Dynamic t a)
uniqDyn initial eUpdate = holdUniqDyn =<< holdDyn initial eUpdate

nodeDyn ::
  forall t m a.
  (Reflex t, MonadHold t m, MonadFix m, Adjustable t m) =>
  Node a ->
  Event t (Node a) ->
  m (NodeD t a, Dynamic t (NodeD t a))
nodeDyn initialNode eNodeUpdated = do
  let
    eNodeUpdated' :: EventSelector t (NodeTag a)
    eNodeUpdated' = fanNode eNodeUpdated
  rec
    initialNodeD <- inner initialNode eNodeUpdated'
    dNodeD <-
      Dom.widgetHold
        (pure initialNodeD)
        (attachWithMaybe
          (\now next ->
             case now of
               NForD{} ->
                 case next of
                   NFor{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NIfThenD{} ->
                 case next of
                   NIfThen{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NIfThenElseD{} ->
                 case next of
                   NIfThenElse{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NPrintD{} ->
                 case next of
                   NPrint{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NReturnD{} ->
                 case next of
                   NReturn{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NDefD{} ->
                 case next of
                   NDef{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NBoolD{} ->
                 case next of
                   NBool{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NIntD{} ->
                 case next of
                   NInt{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NBinOpD{} ->
                 case next of
                   NBinOp{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NUnOpD{} ->
                 case next of
                   NUnOp{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NCallD{} ->
                 case next of
                   NCall{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NListD{} ->
                 case next of
                   NList{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NEIdentD{} ->
                 case next of
                   NEIdent{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NBlockD{} ->
                 case next of
                   NBlock{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NIdentD{} ->
                 case next of
                   NIdent{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NExprsD{} ->
                 case next of
                   NExprs{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NArgsD{} ->
                 case next of
                   NArgs{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NParamsD{} ->
                 case next of
                   NParams{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NSHoleD{} ->
                 case next of
                   NSHole{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NEHoleD{} ->
                 case next of
                   NEHole{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
               NIHoleD{} ->
                 case next of
                   NIHole{} -> Nothing
                   _ -> Just $ inner next eNodeUpdated'
          )
          (current dNodeD)
          eNodeUpdated
        )
  pure (initialNodeD, dNodeD)
  where
    inner :: Node a -> EventSelector t (NodeTag a) -> m (NodeD t a)
    inner initialNode' eNodeUpdated' = do
      case initialNode' of
        NFor i e b ->
          NForD i e b <$>
          uniqDyn i (fmapCheap (view _1) (select eNodeUpdated' NTagFor)) <*>
          uniqDyn e (fmapCheap (view _2) (select eNodeUpdated' NTagFor)) <*>
          uniqDyn b (fmapCheap (view _3) (select eNodeUpdated' NTagFor))
        NIfThen c t ->
          NIfThenD c t <$>
          uniqDyn c (fmapCheap (view _1) (select eNodeUpdated' NTagIfThen)) <*>
          uniqDyn t (fmapCheap (view _2) (select eNodeUpdated' NTagIfThen))
        NIfThenElse c t e ->
          NIfThenElseD c t e <$>
          uniqDyn c (fmapCheap (view _1) (select eNodeUpdated' NTagIfThenElse)) <*>
          uniqDyn t (fmapCheap (view _2) (select eNodeUpdated' NTagIfThenElse)) <*>
          uniqDyn e (fmapCheap (view _3) (select eNodeUpdated' NTagIfThenElse))
        NPrint v ->
          NPrintD v <$>
          uniqDyn v (select eNodeUpdated' NTagPrint)
        NReturn v ->
          NReturnD v <$>
          uniqDyn v (select eNodeUpdated' NTagReturn)
        NDef n ps b ->
          NDefD n ps b <$>
          uniqDyn n (fmapCheap (view _1) (select eNodeUpdated' NTagDef)) <*>
          uniqDyn ps (fmapCheap (view _2) (select eNodeUpdated' NTagDef)) <*>
          uniqDyn b (fmapCheap (view _3) (select eNodeUpdated' NTagDef))

        NBool b ->
          NBoolD b <$>
          uniqDyn b (select eNodeUpdated' NTagBool)
        NInt n ->
          NIntD n <$>
          uniqDyn n (select eNodeUpdated' NTagInt)
        NBinOp op l r ->
          NBinOpD op l r <$>
          uniqDyn op (fmapCheap (view _1) (select eNodeUpdated' NTagBinOp)) <*>
          uniqDyn l (fmapCheap (view _2) (select eNodeUpdated' NTagBinOp)) <*>
          uniqDyn r (fmapCheap (view _3) (select eNodeUpdated' NTagBinOp))
        NUnOp op v ->
          NUnOpD op v <$>
          uniqDyn op (fmapCheap (view _1) (select eNodeUpdated' NTagUnOp)) <*>
          uniqDyn v (fmapCheap (view _2) (select eNodeUpdated' NTagUnOp))
        NCall f x ->
          NCallD f x <$>
          uniqDyn f (fmapCheap (view _1) (select eNodeUpdated' NTagCall)) <*>
          uniqDyn x (fmapCheap (view _2) (select eNodeUpdated' NTagCall))
        NList es ->
          NListD es <$>
          holdDyn es (select eNodeUpdated' NTagList)
        NEIdent i ->
          NEIdentD <$>
          uniqDyn i (select eNodeUpdated' NTagEIdent)

        NBlock bs ->
          NBlockD bs <$>
          holdDyn bs (select eNodeUpdated' NTagBlock)

        NIdent i ->
          NIdentD <$>
          uniqDyn i (select eNodeUpdated' NTagIdent)

        NExprs es ->
          NExprsD es <$>
          holdDyn es (select eNodeUpdated' NTagExprs)
        NArgs es ->
          NArgsD es <$>
          holdDyn es (select eNodeUpdated' NTagArgs)
        NParams es ->
          NParamsD es <$>
          holdDyn es (select eNodeUpdated' NTagParams)

        NSHole -> pure NSHoleD
        NEHole -> pure NEHoleD
        NIHole -> pure NIHoleD
