{-# language GADTs, KindSignatures #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
module Check
  ( CheckT
  , CheckEnv
  , newCheckEnv
  , CheckState
  , newCheckState
  , CheckError(..)
  , runCheckT
  , check
  )
where

import Control.Lens.Getter ((^.), view, to)
import Control.Lens.Setter ((%=), locally)
import Control.Lens.TH (makeLenses)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.State (StateT, runStateT)
import Data.Foldable (traverse_)
import Data.GADT.Show (GShow(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map

import Hash (Hash)
import Node (Node(..))
import Path (Path, Level(..))
import Path.Trie (Trie)
import qualified Path.Trie as Trie
import qualified Path
import Store (MonadStore)
import qualified Store
import Syntax (Expr, Ident(..))

getNode :: MonadStore m => Hash a -> m (Node a)
getNode hash = do
  mNode <- Store.lookupNode hash
  case mNode of
    Nothing -> error $ "missing node for " <> show hash
    Just node -> pure node

data CheckEnv
  = CheckEnv
  { _ceScope :: Map String ()
  }
makeLenses ''CheckEnv

newCheckEnv :: CheckEnv
newCheckEnv =
  CheckEnv
  { _ceScope = mempty
  }

data CheckError :: * -> * where
  NotInScope :: String -> CheckError Expr
deriving instance Show (CheckError a)
instance GShow CheckError where
  gshowsPrec = showsPrec

data CheckState a
  = CheckState
  { _csErrors :: Trie a CheckError
  }
makeLenses ''CheckState

newCheckState :: CheckState a
newCheckState =
  CheckState
  { _csErrors = Trie.empty
  }

newtype CheckT a m x
  = CheckT
  { unCheckT :: MaybeT (ReaderT CheckEnv (StateT (CheckState a) m)) x
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadStore)

instance MonadTrans (CheckT a) where
  lift = CheckT . lift . lift . lift

withScopeEntry ::
  MonadStore m =>
  Hash Ident -> () ->
  CheckT a m x ->
  CheckT a m x
withScopeEntry identHash v m = do
  identNode <- getNode identHash
  case identNode of
    NIHole -> m
    NIdent k -> CheckT $ locally ceScope (Map.insert k v) (unCheckT m)

lookupScopeEntry ::
  Monad m =>
  String ->
  CheckT a m (Maybe ())
lookupScopeEntry k = CheckT $ view (ceScope.to (Map.lookup k))

checkError :: Monad m => Path a b -> CheckError b -> CheckT a m ()
checkError path err =
  CheckT $ csErrors %= Trie.insert path err

runCheckT ::
  Monad m =>
  CheckEnv ->
  CheckState a ->
  CheckT a m x ->
  m (Either (Trie a CheckError) x)
runCheckT env state m = do
  (mRes, state') <- flip runStateT state . flip runReaderT env . runMaybeT $ unCheckT m
  pure $ case mRes of
    Nothing ->
      Left $ state' ^. csErrors
    Just a ->
      let errs = state' ^. csErrors in
      if Trie.null errs
      then Right a
      else Left errs

check :: MonadStore m => Path a b -> Hash b -> CheckT a m ()
check path hash = do
  node <- getNode hash
  case node of
    NFor ident expr body -> do
      check (Path.snoc path For_Expr) expr
      withScopeEntry ident () $ check (Path.snoc path For_Block) body
    NIfThen cond then_ -> do
      check (Path.snoc path IfThen_Cond) cond
      check (Path.snoc path IfThen_Then) then_
    NIfThenElse cond then_ else_ -> do
      check (Path.snoc path IfThenElse_Cond) cond
      check (Path.snoc path IfThenElse_Then) then_
      check (Path.snoc path IfThenElse_Else) else_
    NPrint val ->
      check (Path.snoc path Print_Value) val
    NReturn val ->
      check (Path.snoc path Return_Value) val
    NDef name paramsHash body -> do
      paramsNode <- getNode paramsHash
      case paramsNode of
        NParams params ->
          foldr (\param f -> withScopeEntry param () . f) id params . withScopeEntry name () $
          check (Path.snoc path Def_Body) body
    NBool{} -> pure ()
    NInt{} -> pure ()
    NBinOp _ left right -> do
      check (Path.snoc path BinOp_Left) left
      check (Path.snoc path BinOp_Right) right
    NUnOp _ val ->
      check (Path.snoc path UnOp_Value) val
    NCall func args -> do
      check (Path.snoc path Call_Function) func
      check (Path.snoc path Call_Args) args
    NList exprsHash -> do
      exprsNode <- getNode exprsHash
      case exprsNode of
        NExprs exprs ->
          let path' = Path.snoc path List_Exprs in
          traverse_ (\(ix, st) -> check (Path.snoc path' $ Exprs_Index ix) st) $
          zip [0..] exprs
    NEIdent i -> do
      mEntry <- lookupScopeEntry i
      case mEntry of
        Nothing -> checkError path $ NotInScope i
        Just () -> pure ()

    NBlock sts ->
      traverse_ (\(ix, st) -> check (Path.snoc path $ Block_Index ix) st) $
      zip [0..] (NonEmpty.toList sts)

    NIdent{} -> pure ()

    NExprs xs ->
      traverse_ (\(ix, st) -> check (Path.snoc path $ Exprs_Index ix) st) $
      zip [0..] xs

    NArgs xs ->
      traverse_ (\(ix, st) -> check (Path.snoc path $ Args_Index ix) st) $
      zip [0..] xs

    NParams xs ->
      traverse_ (\(ix, st) -> check (Path.snoc path $ Params_Index ix) st) $
      zip [0..] xs

    NSHole -> pure ()
    NEHole -> pure ()
    NIHole -> pure ()
