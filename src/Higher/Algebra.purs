module Higher.Algebra where

import Prelude

import Higher.NaturalTransformation (NatM)

type HAlgebra (h :: (Type -> Type) -> Type -> Type) (f :: Type -> Type) = h f ~> f

type HAlgebraM (m :: Type -> Type) (h :: (Type -> Type) -> Type -> Type) (f :: Type -> Type) = NatM m (h f) f

type HGAlgebra (w :: (Type -> Type) -> Type -> Type) (h :: (Type -> Type) -> Type -> Type) (a :: Type -> Type) = h (w a) ~> a

type HGAlgebraM (w :: (Type -> Type) -> Type -> Type) (m :: Type -> Type) (h :: (Type -> Type) -> Type -> Type) (a :: Type -> Type) = NatM m (h (w a)) a