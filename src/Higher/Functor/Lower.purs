module Higher.Functor.Lower where

import Prelude

import Higher.Functor (class HFunctor)

newtype Lower f (rec :: Type -> Type) a = Lower (f a)

derive newtype instance functorLower :: Functor f => Functor (Lower f rec)

instance hfunctorLower :: HFunctor (Lower f) where
  hmap _ (Lower f) = Lower f
