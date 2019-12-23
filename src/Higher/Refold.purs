module Higher.Refold where

import Prelude

import Higher.Algebra (HAlgebra, HAlgebraM)
import Higher.Coalgebra (HCoalgebra, HCoalgebraM)
import Higher.Corecursive (class HCorecursive, hembed)
import Higher.Fold (hcata)
import Higher.Functor (class HFunctor, hmap)
import Higher.Recursive (class HRecursive)
import Higher.Traversable (class HTraversable, htraverse)
import Higher.NaturalTransformation (NatM)

hhylo :: forall f a b. HFunctor f => HAlgebra f b -> HCoalgebra f a -> a ~> b
hhylo phi psi = go
  where
    go :: a ~> b
    go t = phi $ hmap go $ psi t

hhyloM :: forall t m h f. HTraversable t => Monad m => HAlgebraM m t h -> HCoalgebraM m t f -> NatM m f h
hhyloM phiM psiM = go
  where
    go :: NatM m f h
    go t = phiM =<< htraverse go =<< psiM t

hconvertTo ∷ forall t f r. HRecursive t f => HCorecursive r f => t ~> r
hconvertTo = hcata hembed