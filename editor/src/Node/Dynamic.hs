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
    Dynamic t (Hash Ident) ->
    Dynamic t (Hash Expr) ->
    Dynamic t (Hash Block) ->
    NodeD t Statement
  NIfThenD ::
    Dynamic t (Hash Expr) ->
    Dynamic t (Hash Block) ->
    NodeD t Statement
  NIfThenElseD ::
    Dynamic t (Hash Expr) ->
    Dynamic t (Hash Block) ->
    Dynamic t (Hash Block) ->
    NodeD t Statement
  NPrintD ::
    Dynamic t (Hash Expr) ->
    NodeD t Statement
  NReturnD ::
    Dynamic t (Hash Expr) ->
    NodeD t Statement
  NDefD ::
    Dynamic t (Hash Ident) ->
    Dynamic t (Hash Params) ->
    Dynamic t (Hash Block) ->
    NodeD t Statement

  NBoolD ::
    Dynamic t Bool ->
    NodeD t Expr
  NIntD ::
    Dynamic t Int ->
    NodeD t Expr
  NBinOpD ::
    Dynamic t BinOp ->
    Dynamic t (Hash Expr) ->
    Dynamic t (Hash Expr) ->
    NodeD t Expr
  NUnOpD ::
    Dynamic t UnOp ->
    Dynamic t (Hash Expr) ->
    NodeD t Expr
  NCallD ::
    Dynamic t (Hash Expr) ->
    Dynamic t (Hash Args) ->
    NodeD t Expr
  NListD ::
    Dynamic t (Hash Exprs) ->
    NodeD t Expr
  NEIdentD ::
    Dynamic t String ->
    NodeD t Expr

  NBlockD ::
    Dynamic t (NonEmpty (Hash Statement)) ->
    NodeD t Block

  NIdentD ::
    Dynamic t String ->
    NodeD t Ident

  NExprsD ::
    Dynamic t [Hash Expr] ->
    NodeD t Exprs
  NArgsD ::
    Dynamic t [Hash Expr] ->
    NodeD t Args
  NParamsD ::
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
  Dynamic t (Node a) ->
  m (Dynamic t (NodeD t a))
nodeDyn dNode = do
  let fanned = fanNode $ updated dNode
  rec
    dNodeD <-
      Dom.widgetHold
        (inner fanned dNode)
        (attachWithMaybe
          (\now next ->
             case now of
               NForD{} ->
                 case next of
                   NFor{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NIfThenD{} ->
                 case next of
                   NIfThen{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NIfThenElseD{} ->
                 case next of
                   NIfThenElse{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NPrintD{} ->
                 case next of
                   NPrint{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NReturnD{} ->
                 case next of
                   NReturn{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NDefD{} ->
                 case next of
                   NDef{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NBoolD{} ->
                 case next of
                   NBool{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NIntD{} ->
                 case next of
                   NInt{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NBinOpD{} ->
                 case next of
                   NBinOp{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NUnOpD{} ->
                 case next of
                   NUnOp{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NCallD{} ->
                 case next of
                   NCall{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NListD{} ->
                 case next of
                   NList{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NEIdentD{} ->
                 case next of
                   NEIdent{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NBlockD{} ->
                 case next of
                   NBlock{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NIdentD{} ->
                 case next of
                   NIdent{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NExprsD{} ->
                 case next of
                   NExprs{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NArgsD{} ->
                 case next of
                   NArgs{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NParamsD{} ->
                 case next of
                   NParams{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NSHoleD{} ->
                 case next of
                   NSHole{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NEHoleD{} ->
                 case next of
                   NEHole{} -> Nothing
                   _ -> Just $ inner fanned dNode
               NIHoleD{} ->
                 case next of
                   NIHole{} -> Nothing
                   _ -> Just $ inner fanned dNode
          )
          (current dNodeD)
          (updated dNode)
        )
  pure dNodeD
  where
    inner :: EventSelector t (NodeTag a) -> Dynamic t (Node a) -> m (NodeD t a)
    inner fanned dNode' = do
      node <- sample $ current dNode'
      case node of
        NFor i e b ->
          NForD <$>
          uniqDyn i (fmapCheap (view _1) (select fanned NTagFor)) <*>
          uniqDyn e (fmapCheap (view _2) (select fanned NTagFor)) <*>
          uniqDyn b (fmapCheap (view _3) (select fanned NTagFor))
        NIfThen c t ->
          NIfThenD <$>
          uniqDyn c (fmapCheap (view _1) (select fanned NTagIfThen)) <*>
          uniqDyn t (fmapCheap (view _2) (select fanned NTagIfThen))
        NIfThenElse c t e ->
          NIfThenElseD <$>
          uniqDyn c (fmapCheap (view _1) (select fanned NTagIfThenElse)) <*>
          uniqDyn t (fmapCheap (view _2) (select fanned NTagIfThenElse)) <*>
          uniqDyn e (fmapCheap (view _3) (select fanned NTagIfThenElse))
        NPrint v ->
          NPrintD <$>
          uniqDyn v (select fanned NTagPrint)
        NReturn v ->
          NReturnD <$>
          uniqDyn v (select fanned NTagReturn)
        NDef n ps b ->
          NDefD <$>
          uniqDyn n (fmapCheap (view _1) (select fanned NTagDef)) <*>
          uniqDyn ps (fmapCheap (view _2) (select fanned NTagDef)) <*>
          uniqDyn b (fmapCheap (view _3) (select fanned NTagDef))

        NBool b ->
          NBoolD <$>
          uniqDyn b (select fanned NTagBool)
        NInt n ->
          NIntD <$>
          uniqDyn n (select fanned NTagInt)
        NBinOp op l r ->
          NBinOpD <$>
          uniqDyn op (fmapCheap (view _1) (select fanned NTagBinOp)) <*>
          uniqDyn l (fmapCheap (view _2) (select fanned NTagBinOp)) <*>
          uniqDyn r (fmapCheap (view _3) (select fanned NTagBinOp))
        NUnOp op v ->
          NUnOpD <$>
          uniqDyn op (fmapCheap (view _1) (select fanned NTagUnOp)) <*>
          uniqDyn v (fmapCheap (view _2) (select fanned NTagUnOp))
        NCall f x ->
          NCallD <$>
          uniqDyn f (fmapCheap (view _1) (select fanned NTagCall)) <*>
          uniqDyn x (fmapCheap (view _2) (select fanned NTagCall))
        NList es ->
          NListD <$>
          holdDyn es (select fanned NTagList)
        NEIdent i ->
          NEIdentD <$>
          uniqDyn i (select fanned NTagEIdent)

        NBlock bs ->
          NBlockD <$>
          holdDyn bs (select fanned NTagBlock)

        NIdent i ->
          NIdentD <$>
          uniqDyn i (select fanned NTagIdent)

        NExprs es ->
          NExprsD <$>
          holdDyn es (select fanned NTagExprs)
        NArgs es ->
          NArgsD <$>
          holdDyn es (select fanned NTagArgs)
        NParams es ->
          NParamsD <$>
          holdDyn es (select fanned NTagParams)

        NSHole -> pure NSHoleD
        NEHole -> pure NEHoleD
        NIHole -> pure NIHoleD
