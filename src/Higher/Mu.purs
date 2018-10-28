module Higher.Mu where

import Higher.Corecursive (class HCorecursive)
import Higher.Functor (class HFunctor)
import Higher.Recursive (class HRecursive)

newtype HMu f a = HMu (f (HMu f) a)

instance hrecursiveMu :: HFunctor f => HRecursive (HMu f) f where
  hproject (HMu a) = a

instance hcorecursiveMu :: HFunctor f => HCorecursive (HMu f) f where
  hembed = HMu
