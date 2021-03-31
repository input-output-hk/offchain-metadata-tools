{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Metadata.Transform.Reader
  ( Transform
  , mkTransform
  , apply
  , withInput
  ) where

import           Control.Applicative  (Alternative, empty, (<|>))
import           Control.Comonad
import           Control.Monad.Reader (ReaderT (ReaderT), runReaderT,
                                       withReaderT)

data Transform r f a = Transform (ReaderT r f a)

instance Functor f => Functor (Transform r f) where
  fmap f (Transform reader) = Transform $ fmap f reader

instance Applicative f => Applicative (Transform r f) where
  pure = Transform . pure

  (Transform ff) <*> (Transform fa) = Transform $ ff <*> fa

instance Monad f => Monad (Transform r f) where
  (Transform fa) >>= fmb =
    Transform $ fa >>= (ReaderT . apply . fmb)

instance Alternative f => Alternative (Transform r f) where
  empty = Transform empty
  (Transform f1) <|> (Transform f2) = Transform $ f1 <|> f2

instance (Monoid r, Comonad f) => Comonad (Transform r f) where
  extract trans = extract $ trans `apply` mempty

  duplicate (Transform (ReaderT f)) =
    let
      -- base :: f (f a)
      base = (duplicate $ extract f)
    in
      mkTransform (const $ (mkTransform . const) <$> base)

apply :: Transform r f a -> r -> f a
apply (Transform reader) = runReaderT reader

mkTransform :: (r -> f a) -> Transform r f a
mkTransform = Transform . ReaderT

withInput :: (r' -> r) -> Transform r f a -> Transform r' f a
withInput f (Transform readerR) = Transform $ withReaderT f readerR
