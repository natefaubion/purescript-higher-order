module Higher.Data.Traversable where

import Prelude

import Higher.Data.Foldable (class HFoldable)
import Higher.NaturalTransformation (NatM)

class HFoldable h <= HTraversable (h :: (Type -> Type) -> (Type -> Type))  where
  -- hsequence
  htraverse :: forall f g e. Applicative e => NatM e f g -> NatM e (h f) (h g)