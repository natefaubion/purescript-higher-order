module Higher.Functor.Mu where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Plus (class Plus, empty)
import Data.Eq (class Eq1, eq1)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1, compare1)

newtype HMu (f :: (Type -> Type) -> (Type -> Type)) (a :: Type) = HMu (f (HMu f) a)

derive instance newtypeHMu :: Newtype (HMu f a) _

instance eqHMu :: (Eq a, Eq1 (f (HMu f))) => Eq (HMu f a) where
  eq (HMu x) (HMu y) = eq1 x y

instance ordHMu :: (Ord a, Ord1 (f (HMu f))) => Ord (HMu f a) where
  compare (HMu x) (HMu y) = compare1 x y

instance semigroupHMu :: Alt (f (HMu f)) => Semigroup (HMu f a) where
  append (HMu x) (HMu y) = HMu (x <|> y)

instance monoidHMu :: Plus (f (HMu f)) => Monoid (HMu f a) where
  mempty = HMu empty