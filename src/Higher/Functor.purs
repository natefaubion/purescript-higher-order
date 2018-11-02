module Higher.Functor where

import Prelude

class HFunctor (f :: (Type -> Type) -> (Type -> Type))  where
  hmap :: forall a b. (a ~> b) -> f a ~> f b
