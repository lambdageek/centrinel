-- | Just the smallest set of lens operators
{-# language RankNTypes #-}
module ZeptoLens where

import Control.Applicative (Const (..))
import Control.Monad.Reader.Class
import Data.Functor.Identity (Identity (..))

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)
{-# INLINE over #-}

(%~) :: Lens s t a b -> (a -> b) -> s -> t
(%~) = over
{-# INLINE (%~) #-}

set :: Lens s t a b -> b -> s -> t
set l b = over l (const b)
{-# INLINE set #-}

(.~) :: Lens s t a b -> b -> s -> t
(.~) = set
{-# INLINE (.~) #-}

view :: MonadReader s m => Lens s t a b -> m a
view l = asks (getConst . l Const)
{-# INLINE view #-}

views :: MonadReader s m => Lens s t a b -> (a -> r) -> m r
views l f = asks (getConst . l (Const . f))
{-# INLINE views #-}
