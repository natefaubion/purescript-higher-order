module Higher.Corecursive where

import Data.Newtype (wrap)
import Higher.Algebra (HAlgebra)
import Higher.Functor (class HFunctor)
import Higher.Functor.Mu (HMu)

class HFunctor f <= HCorecursive t f | t -> f where
  hembed :: HAlgebra f t

instance hcorecursiveMu :: HFunctor f => HCorecursive (HMu f) f where
  hembed = wrap
