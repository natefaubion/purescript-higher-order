module Higher.Recursive where

import Prelude

import Higher.Functor (class HFunctor)

class HFunctor f <= HRecursive t f | t -> f where
  hproject :: t ~> f t