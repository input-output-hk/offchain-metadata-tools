{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Metadata.Transform.Reader
  ( Transform
  , mkTransform
  , apply
  , withInput
  ) where

import Control.Applicative
    ( Alternative )
import Control.Comonad
import Control.Monad.Reader
    ( ReaderT (ReaderT) )

newtype Transform r f a = Transform (r -> f a)
  deriving (Functor, Applicative, Alternative, Monad)
  via ReaderT r f

instance (Monoid r, Comonad f) => Comonad (Transform r f) where
  extract trans = extract $ trans `apply` mempty

  duplicate (Transform f) =
    let
      -- base :: f (f a)
      base = (duplicate $ extract f)
    in
      mkTransform (const $ (mkTransform . const) <$> base)

apply :: Transform r f a -> r -> f a
apply (Transform reader) = reader

mkTransform :: (r -> f a) -> Transform r f a
mkTransform = Transform

withInput :: (r' -> r) -> Transform r f a -> Transform r' f a
withInput f (Transform readerR) = Transform $ readerR . f
