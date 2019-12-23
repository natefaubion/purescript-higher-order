module Higher.Fold where

import Prelude

import Control.Apply (lift2)
import Data.Functor.Product (Product, product)
import Higher.Algebra (HAlgebra, HAlgebraM, HGAlgebra, HGAlgebraM)
import Higher.Corecursive (class HCorecursive, hembed)
import Higher.Recursive (class HRecursive, hproject)
import Higher.Coalgebra (HCoalgebra)
import Higher.Functor (class HFunctor, hmap)
import Higher.Traversable (class HTraversable, htraverse)
import Higher.NaturalTransformation (NatM)

hcata :: forall h f t. HFunctor h => HRecursive t h => HAlgebra h f -> t ~> f
hcata phi = go
  where
    go :: t ~> f
    go t = phi $ hmap go $ hproject t

hcataM :: forall f t m a. HTraversable f => Monad m => HRecursive t f => HAlgebraM m f a -> NatM m t a
hcataM phiM = go
  where
    go :: NatM m t a
    go t = phiM =<< htraverse go (hproject t)

hlambek ∷ forall t f. HRecursive t f => HCorecursive t f => HCoalgebra f t
hlambek = hcata (hmap hembed)

hpara :: forall h t a. HFunctor h => HRecursive t h => HGAlgebra (Product t) h a -> t ~> a
hpara phi = go
  where
    go :: t ~> a
    go t = phi $ hmap (\a -> product a (go a)) $ hproject t

hparaM :: forall h t m a. HTraversable t => HRecursive h t => Monad m => HGAlgebraM (Product h) m t a -> NatM m h a
hparaM phiM = go
  where
    go :: NatM m h a
    go t = phiM =<< htraverse (\a -> lift2 product (pure a) (go a)) (hproject t)
