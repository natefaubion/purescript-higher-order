module Higher.Class.Recursive where

import Data.Newtype (unwrap)
import Higher.Coalgebra (HCoalgebra)
import Higher.Data.Functor (class HFunctor)
import Higher.Functor.Mu (HMu)

class HFunctor f <= HRecursive t f | t -> f where
  hproject :: HCoalgebra f t

instance hrecursiveMu :: HFunctor f => HRecursive (HMu f) f where
  hproject = unwrap 