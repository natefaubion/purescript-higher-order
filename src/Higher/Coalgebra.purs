module Higher.Coalgebra where

import Prelude

import Higher.NaturalTransformation (NatM)

type HCoalgebra h f = f ~> h f

type HCoalgebraM m h f = NatM m f (h f)