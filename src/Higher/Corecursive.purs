module Higher.Corecursive where

import Prelude

import Higher.Functor (class HFunctor)

class HFunctor f <= HCorecursive t f | t -> f where
  hembed :: f t ~> t
