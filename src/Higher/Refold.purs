module Higher.Refold where

import Prelude

import Higher.Algebra (HAlgebra, HAlgebraM)
import Higher.Corecursive (class HCorecursive, hembed)
import Higher.Recursive (class HRecursive)
import Higher.Coalgebra (HCoalgebra, HCoalgebraM)
import Higher.Functor (class HFunctor, hmap)
import Higher.Traversable (class HTraversable, htraverse)
import Higher.Fold (hcata)

hhylo :: forall f a b. HFunctor f => HAlgebra f b -> HCoalgebra f a -> a ~> b
hhylo phi psi = phi <<< hmap (hhylo phi psi) <<< psi

hhyloM :: forall t m h f a. HTraversable t => Monad m => HAlgebraM m t h -> HCoalgebraM m t f -> f a -> m (h a)
hhyloM phiM psiM = phiM <=< htraverse (hhyloM phiM psiM) <=< psiM

hconvertTo ∷ forall t f r. HRecursive t f => HCorecursive r f => t ~> r
hconvertTo = hcata hembed