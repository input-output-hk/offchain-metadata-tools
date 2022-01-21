{- |
An algebra for describing transformations and their combination.

Mapping, combining, and sequencing are provided via the Functor,
Applicative, and Monad typeclasses (respectively).

The property we are specifically looking for with this type is that
"combining" two transformations (using the Applicative typeclass) is
the same as applying the input to both transforms and then combining
the result:

  ∀t1 t2 input. apply (t1 *> t2) input = apply t1 input *> apply t2 input.

Importantly, plain-old functions (->) DO NOT satisfy this constraint.
Hence the definition of a new "function-like" type.
-}

module Cardano.Metadata.Transform
  ( -- * Types
    Transform
  , Transform_
    -- * Constructors
  , mkTransform
  , mkTransform_
    -- * Observations
  , apply
  , apply_
  ) where

import Data.Functor.Identity ( Identity, runIdentity )

import qualified Cardano.Metadata.Transform.Reader as Impl

-- | Type representing a transformation.
type Transform r f a = Impl.Transform r f a

-- | Type representing a transformation whose result has no additional context.
type Transform_ r a = Transform r Identity a

-- | Apply a transform to the input to get a result.
apply :: Transform r f a -> r -> f a
apply = Impl.apply

-- | Same as @apply@ but transform result has no additional context.
apply_ :: Transform_ r a -> r -> a
apply_ t = runIdentity . apply t

-- | Create a transform from a transform function.
mkTransform :: (r -> f a) -> Transform r f a
mkTransform = Impl.mkTransform

-- | Same as @mkTransform@ but result has no additional context.
mkTransform_ :: (r -> a) -> Transform_ r a
mkTransform_ f = mkTransform (pure . f)
