module Higher.Refold where

import Prelude

import Higher.Algebra (HAlgebra, HAlgebraM)
import Higher.Class.Corecursive (class HCorecursive, hembed)
import Higher.Class.Recursive (class HRecursive)
import Higher.Coalgebra (HCoalgebra, HCoalgebraM)
import Higher.Data.Functor (class HFunctor, hmap)
import Higher.Data.Traversable (class HTraversable, htraverse)
import Higher.Fold (hcata)

hhylo :: forall f a b. HFunctor f => HAlgebra f b -> HCoalgebra f a -> a ~> b
hhylo f g = f <<< hmap (hhylo f g) <<< g

hhyloM :: forall t m h f a. HTraversable t => Monad m => HAlgebraM m t h -> HCoalgebraM m t f -> f a -> m (h a)
hhyloM f g = f <=< htraverse (hhyloM f g) <=< g

hconvertTo ∷ forall t f r. HRecursive t f => HCorecursive r f => t ~> r
hconvertTo = hcata hembed