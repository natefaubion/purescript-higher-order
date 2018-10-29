module Higher.Fold where

import Prelude

import Control.Apply (lift2)
import Data.Functor.Product (Product, product)
import Higher.Algebra (HAlgebra, HAlgebraM, HGAlgebra, HGAlgebraM)
import Higher.Class.Corecursive (class HCorecursive, hembed)
import Higher.Class.Recursive (class HRecursive, hproject)
import Higher.Coalgebra (HCoalgebra)
import Higher.Data.Functor (class HFunctor, hmap)
import Higher.Data.Traversable (class HTraversable, htraverse)
import Higher.NaturalTransformation (NatM)

hcata :: forall h f t. HFunctor h => HRecursive t h => HAlgebra h f -> t ~> f
hcata phi = phi <<< hmap (hcata phi) <<< hproject

hcataM :: forall f t m a. HTraversable f => Monad m => HRecursive t f => HAlgebraM m f a -> NatM m t a
hcataM phiM = phiM <=< htraverse (hcataM phiM) <<< hproject

hlambek ∷ forall t f. HRecursive t f => HCorecursive t f => HCoalgebra f t
hlambek = hcata (hmap hembed)

hpara :: forall h t a. HFunctor h => HRecursive t h => HGAlgebra (Product t) h a -> t ~> a
hpara phi = phi <<< hmap (\a -> product a (hpara phi a)) <<< hproject

hparaM :: forall h t m a. HTraversable t => HRecursive h t => Monad m => HGAlgebraM (Product h) m t a -> NatM m h a
hparaM phiM = phiM <=< htraverse (\a -> lift2 product (pure a) (hparaM phiM a)) <<< hproject