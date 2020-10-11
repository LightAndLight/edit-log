{-# language GADTs #-}
{-# language StandaloneDeriving #-}
module Focus where

import NodeType (KnownNodeType)
import Path (Path)

data Focus a where
  Focus :: KnownNodeType b => Path a b -> Focus a
  NoFocus :: Focus a

deriving instance Show (Focus a)
