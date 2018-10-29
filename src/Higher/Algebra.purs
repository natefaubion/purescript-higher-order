module Higher.Algebra where

import Prelude

import Higher.NaturalTransformation (NatM)

type HAlgebra h f = h f ~> f

type HAlgebraM m h f = NatM m (h f) f