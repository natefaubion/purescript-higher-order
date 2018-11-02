module Higher.Foldable where

import Prelude

import Data.Monoid.Dual (Dual(..))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Higher.Functor (class HFunctor)
import Higher.NaturalTransformation (type (~>.))

class HFunctor h <= HFoldable (h :: (Type -> Type) -> (Type -> Type)) where
  hfoldr :: forall a b. (a ~>. (b -> b)) -> b -> h a ~>. b
  hfoldl :: forall a b. (b -> (a ~>. b)) -> b -> h a ~>. b
  hfoldMap :: forall f m. Monoid m => (f ~>. m) -> h f ~>. m

hfoldlDefault :: forall h a b. HFoldable h => (b -> (a ~>. b)) -> b -> h a ~>. b
hfoldlDefault f z t = unwrap (unwrap (hfoldMap (Dual <<< Endo <<< flip f) t)) z

hfoldrDefault :: forall h a b. HFoldable h => (a ~>. (b -> b)) -> b -> h a ~>. b
hfoldrDefault f z t = unwrap (hfoldMap (Endo <<< f) t) z

hfoldMapDefaultR :: forall f h m. HFoldable h => Monoid m => (f ~>. m) -> h f ~>. m
hfoldMapDefaultR f = hfoldr (\x acc -> f x <> acc) mempty

hfoldMapDefaultL :: forall f h m. HFoldable h => Monoid m => (f ~>. m) -> h f ~>. m
hfoldMapDefaultL f = hfoldl (\acc x -> acc <> f x) mempty
