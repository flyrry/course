{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  --(<$>) :: (a -> b) -> Compose f g a -> Compose f g b
  (<$>) fn (Compose a) = Compose ((fn <$>) <$> a)

instance (Apply f, Apply g) =>
  Apply (Compose f g) where
-- Implement the (<*>) function for an Apply instance for Compose
  --(<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (<*>) cab ca = lift2 (\f a -> f a) cab ca
  --(<*>) = error "todo"

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure = Compose . pure . pure

instance (Bind f, Bind g) =>
  Bind (Compose f g) where
-- Implement the (=<<) function for a Bind instance for Compose
  --(=<<) :: (a -> Compose f g b) -> Compose f g a -> Compose f g b
  (=<<) = error "todo"
