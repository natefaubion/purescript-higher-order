module Higher.Functor where

import Prelude

class HFunctor f where
  hmap :: forall a b. (a ~> b) -> f a ~> f b
