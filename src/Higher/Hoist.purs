module Higher.Hoist where

import Prelude

import Higher.Class.Corecursive (class HCorecursive, hembed)
import Higher.Class.Recursive (class HRecursive)
import Higher.Data.Functor (class HFunctor)
import Higher.Fold (hcata)
import Higher.Functor.Mu (HMu)

gHoist :: forall t f g a. HRecursive t f => HCorecursive a g => (f a ~> g a) -> t ~> a
gHoist n t = hcata (\x -> hembed (n x)) t

hoistHMu :: forall f g. HFunctor f => HFunctor g => (f (HMu g) ~> g (HMu g)) -> HMu f ~> HMu g
hoistHMu = gHoist