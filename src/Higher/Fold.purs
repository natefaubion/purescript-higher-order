module Higher.Fold where

import Prelude

import Data.Functor.Product (Product, product)
import Higher.Algebra (HAlgebra, HAlgebraM, HGAlgebra)
import Higher.Class.Corecursive (class HCorecursive, hembed)
import Higher.Class.Recursive (class HRecursive, hproject)
import Higher.Coalgebra (HCoalgebra)
import Higher.Data.Functor (class HFunctor, hmap)
import Higher.Data.Traversable (class HTraversable, htraverse)
import Higher.NaturalTransformation (NatM)

hcata :: forall h f t. HFunctor h => HRecursive t h => HAlgebra h f -> t ~> f
hcata phi = phi <<< hmap (hcata phi) <<< hproject

hcataM :: forall f t m a. HTraversable f => Monad m => HRecursive t f => HAlgebraM m f a -> NatM m t a
hcataM f = f <=< htraverse (hcataM f) <<< hproject

hlambek ∷ forall t f. HRecursive t f => HCorecursive t f => HCoalgebra f t
hlambek = hcata (hmap hembed)

hpara :: forall h t a. HFunctor h => HRecursive t h => HGAlgebra (Product t) h a -> t ~> a
hpara f = f <<< hmap (\a -> product a (hpara f a)) <<< hproject