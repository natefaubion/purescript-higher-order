module Higher.Traversable where

import Prelude

import Higher.Foldable (class HFoldable)
import Higher.NaturalTransformation (NatM)

class HFoldable h <= HTraversable (h :: (Type -> Type) -> (Type -> Type))  where
  htraverse :: forall f g e. Applicative e => NatM e f g -> NatM e (h f) (h g)
