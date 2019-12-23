module Higher.Free
  ( HFreeF
  , HFree(..)
  , HFreeViewF(..)
  , HFreeView(..)
  , resume
  , view
  , run
  , runRec
  , runPure
  , interpret
  , interpretRec
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Exists (Exists, mkExists, runExists)
import Data.Newtype (class Newtype)
import Higher.Corecursive (class HCorecursive, hembed)
import Higher.Functor (class HFunctor, hmap)
import Higher.Functor.Mu (HMu(..))
import Higher.Recursive (class HRecursive, hproject)
import Unsafe.Coerce (unsafeCoerce)

foreign import data UnsafeExists :: Type

data HFreeF f rec a
  = Bind (f rec UnsafeExists) (FreeBinds rec UnsafeExists a)
  | Pure a

newtype HFree f a = HFree (HMu (HFreeF f) a)

data HFreeViewF fix f a b
  = BindView (f (fix f) b) (b -> fix f a)
  | PureView a

newtype HFreeView fix f a = HFreeView (Exists (HFreeViewF fix f a))

-- Based on http://okmij.org/ftp/Haskell/extensible/FTCQueue1.hs
data FreeBinds f a b
  = Leaf (a -> f b)
  | Node (FreeBinds f a UnsafeExists) (FreeBinds f UnsafeExists b)

data FreeCons f a b
  = FreeCons (a -> f UnsafeExists) (FreeBinds f UnsafeExists b)

unconsBinds ::
  forall f a b x.
  FreeBinds f a x ->
  FreeBinds f x b ->
  FreeCons f a b
unconsBinds l r = case l of
  Leaf k ->
    FreeCons
      ((unsafeCoerce :: (a -> f x) -> a -> f UnsafeExists) k)
      ((unsafeCoerce :: FreeBinds f x b -> FreeBinds f UnsafeExists b) r)
  Node l' r' ->
    unconsBinds l'
      (Node
        ((unsafeCoerce :: FreeBinds f UnsafeExists x -> FreeBinds f UnsafeExists UnsafeExists) r')
        ((unsafeCoerce :: FreeBinds f x b -> FreeBinds f UnsafeExists b) r))

hoistBinds ::
  forall f g a b.
  (f ~> g) ->
  FreeBinds f a b ->
  FreeBinds g a b
hoistBinds f = go
  where
  go :: forall x y. FreeBinds f x y -> FreeBinds g x y
  go = case _ of
    Leaf k -> Leaf (k >>> f)
    Node l r -> Node (go l) (go r)

derive instance newtypeHFree :: Newtype (HFree f a) _

instance hfunctorHFreeF :: HFunctor f => HFunctor (HFreeF f) where
  hmap f = case _ of
    Bind a bs -> Bind (hmap f a) (hoistBinds f bs)
    Pure a -> Pure a

instance hrecursiveHFree :: HFunctor f => HRecursive (HFree f) (HFreeF f) where
  hproject = unsafeCoerce

instance hcorecursiveHFree :: HFunctor f => HCorecursive (HFree f) (HFreeF f) where
  hembed = unsafeCoerce

instance functorHFree :: Functor (HFree f) where
  map = mapImpl

instance applyHFree :: Apply (HFree f) where
  apply = ap

instance applicativeHFree :: Applicative (HFree f) where
  pure a = HFree (HMu (Pure a))

instance bindHFree :: Bind (HFree f) where
  bind = bindImpl

instance monadHFree :: Monad (HFree f)

mapImpl :: forall f a b. (a -> b) -> HFree f a -> HFree f b
mapImpl k (HFree (HMu ma)) = HFree (HMu mb)
  where
  mb = case ma of
    Pure a -> Pure (k a)
    Bind f bs -> Bind f (Node (unsafeCoerce bs) (Leaf (unsafeCoerce (HMu <<< Pure <<< k))))

bindImpl :: forall f a b. HFree f a -> (a -> HFree f b) -> HFree f b
bindImpl (HFree (HMu ma)) k = case ma of
  Pure a -> k a
  Bind f bs -> HFree (HMu (Bind f (Node (unsafeCoerce bs) (Leaf (unsafeCoerce k)))))

resume ::
  forall fix f a r.
  HRecursive (fix f) (HFreeF f) =>
  HCorecursive (fix f) (HFreeF f) =>
  (forall b. f (fix f) b -> (b -> fix f a) -> r) ->
  (a -> r) ->
  fix f a ->
  r
resume bind' pure' =
  hproject >>> case _ of
    Bind a bs -> bind' a (go bs)
    Pure a -> pure' a
  where
  go :: forall x y. FreeBinds (fix f) x y -> x -> fix f y
  go bs x = case bs of
    Leaf k -> k x
    Node l r -> case unconsBinds l r of
      FreeCons k bs' -> case hproject (k x) of
        Pure a -> go bs' a
        Bind a bs'' -> hembed (Bind a (Node bs'' bs'))

view ::
  forall fix f a.
  HRecursive (fix f) (HFreeF f) =>
  HCorecursive (fix f) (HFreeF f) =>
  fix f a ->
  HFreeView fix f a
view = resume (\a b -> HFreeView (mkExists (BindView a b))) (HFreeView <<< mkExists <<< PureView)

run ::
  forall fix f m a.
  HRecursive (fix f) (HFreeF f) =>
  HCorecursive (fix f) (HFreeF f) =>
  Functor (f (fix f)) =>
  Monad m =>
  (f (fix f) (fix f a) -> m (fix f a)) ->
  fix f a ->
  m a
run next = go where go = resume (\f k -> next (k <$> f) >>= go) pure

runRec ::
  forall fix f m a.
  HRecursive (fix f) (HFreeF f) =>
  HCorecursive (fix f) (HFreeF f) =>
  Functor (f (fix f)) =>
  MonadRec m =>
  (f (fix f) (fix f a) -> m (fix f a)) ->
  fix f a ->
  m a
runRec next = tailRecM go <<< view
  where
  go (HFreeView v) = v # runExists case _ of
    PureView a -> pure $ Done a
    BindView f k -> Loop <<< view <$> next (k <$> f)

runPure ::
  forall fix f a.
  HRecursive (fix f) (HFreeF f) =>
  HCorecursive (fix f) (HFreeF f) =>
  Functor (f (fix f)) =>
  (f (fix f) (fix f a) -> fix f a) ->
  fix f a ->
  a
runPure next = go
  where
  go :: fix f a -> a
  go x = case unsafeCoerce (view x) :: HFreeViewF fix f a UnsafeExists of
    PureView a -> a
    BindView f k -> go (next (k <$> f))

interpret ::
  forall fix f m.
  HRecursive (fix f) (HFreeF f) =>
  HCorecursive (fix f) (HFreeF f) =>
  Monad m =>
  (f (fix f) ~> m) ->
  fix f ~>
  m
interpret next = go where go = resume (\f k -> next f >>= k >>> go) pure

interpretRec ::
  forall fix f m.
  HRecursive (fix f) (HFreeF f) =>
  HCorecursive (fix f) (HFreeF f) =>
  MonadRec m =>
  (f (fix f) ~> m) ->
  fix f ~>
  m
interpretRec next = tailRecM go <<< view
  where
  go (HFreeView v) = v # runExists case _ of
    PureView a -> pure $ Done a
    BindView f k -> Loop <<< view <<< k <$> next f
