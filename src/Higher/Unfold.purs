module Higher.Unfold where

import Prelude

import Higher.Algebra (HAlgebra)
import Higher.Class.Corecursive (class HCorecursive, hembed)
import Higher.Class.Recursive (class HRecursive, hproject)
import Higher.Coalgebra (HCoalgebra, HCoalgebraM)
import Higher.Data.Functor (class HFunctor, hmap)
import Higher.Data.Traversable (class HTraversable, htraverse)
import Higher.NaturalTransformation (NatM)

hana :: forall h f t. HFunctor h => HCorecursive t h => HCoalgebra h f -> f ~> t
hana psi = hembed <<< hmap (hana psi) <<< psi

hanaM :: forall f t m a. HTraversable f => Monad m => HCorecursive t f => HCoalgebraM m f a -> NatM m a t
hanaM f = map hembed <<< htraverse (hanaM f) <=< f

hcolambek ∷ forall t f. HRecursive t f => HCorecursive t f => HAlgebra f t
hcolambek = hana (hmap hproject)