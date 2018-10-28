module Higher.NaturalTransformation where

infixr 4 type NaturalTransformationK as ~>.

type NatM m f g = forall a. f a -> m (g a)
type NaturalTransformationK f a = forall i. f i -> a