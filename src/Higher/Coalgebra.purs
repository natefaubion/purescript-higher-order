module Higher.Coalgebra where

import Prelude

import Higher.NaturalTransformation (NatM)

type HCoalgebra (h :: (Type -> Type) -> Type -> Type) (f :: Type -> Type) = f ~> h f

type HCoalgebraM (m :: Type -> Type) (h :: (Type -> Type) -> Type -> Type) (f :: Type -> Type) = NatM m f (h f)

type HGCoalgebra (m :: (Type -> Type) -> Type -> Type) (h :: (Type -> Type) -> Type -> Type) (a :: Type -> Type) = a ~> h (m a)