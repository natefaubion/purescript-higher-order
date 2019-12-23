module Higher.Unfold where

import Prelude

import Data.Functor.Coproduct (Coproduct, coproduct)
import Higher.Algebra (HAlgebra)
import Higher.Corecursive (class HCorecursive, hembed)
import Higher.Recursive (class HRecursive, hproject)
import Higher.Coalgebra (HCoalgebra, HCoalgebraM, HGCoalgebra, HGCoalgebraM)
import Higher.Functor (class HFunctor, hmap)
import Higher.Traversable (class HTraversable, htraverse)
import Higher.NaturalTransformation (NatM)

hana :: forall h f t. HFunctor h => HCorecursive t h => HCoalgebra h f -> f ~> t
hana psi = go
  where
    go :: f ~> t
    go t = hembed $ hmap go $ psi t

hanaM :: forall f t m a. HTraversable f => Monad m => HCorecursive t f => HCoalgebraM m f a -> NatM m a t
hanaM psiM = go
  where
    go :: NatM m a t
    go t = map hembed $ htraverse go =<< psiM t

hcolambek ∷ forall t f. HRecursive t f => HCorecursive t f => HAlgebra f t
hcolambek = hana (hmap hproject)

hapo :: forall t h a. HCorecursive t h => HGCoalgebra (Coproduct t) h a -> a ~> t
hapo psi = go
  where
    go :: a ~> t
    go t = hembed $ hmap (coproduct identity go) $ psi t

hapoM :: forall h t m a. HCorecursive h t => HTraversable t => Monad m => HGCoalgebraM (Coproduct h) m t a -> NatM m a h
hapoM psiM = go
  where
    go :: NatM m a h
    go t = map hembed $ htraverse (coproduct pure go) =<< psiM t
