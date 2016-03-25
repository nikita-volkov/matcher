module Matcher
(
  Matcher,
  run,
  equals,
  satisfies,
  converts,
  whatever,
)
where

import Matcher.Prelude
import qualified Success.Pure


-- |
-- Converts the matcher into a conversion function,
-- which results in either a successful result or a failure.
run :: Matcher a b -> a -> Either Text b
run (Matcher (ReaderT successFn)) input =
  either (Left . fromMaybe "") Right $
  Success.Pure.asEither $
  successFn input


-- |
-- A composable abstraction for checking or converting a context value.
newtype Matcher a b =
  Matcher (ReaderT a (Success Text) b)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance Profunctor Matcher where
  {-# INLINE lmap #-}
  lmap fn (Matcher (ReaderT successFn)) =
    Matcher (ReaderT (successFn . fn))
  {-# INLINE rmap #-}
  rmap =
    fmap

instance Category Matcher where
  {-# INLINE id #-}
  id =
    Matcher $
    ReaderT $
    Success.Pure.success
  {-# INLINE (.) #-}
  (.) (Matcher (ReaderT successFn2)) (Matcher (ReaderT successFn1)) =
    Matcher $
    ReaderT $
    successFn1 >=> successFn2

instance Arrow Matcher where
  {-# INLINE arr #-}
  arr f =
    Matcher $
    ReaderT $
    Success.Pure.success . f
  {-# INLINABLE first #-}
  first (Matcher (ReaderT successFn)) =
    Matcher $
    ReaderT $
    \(a, b) ->
      fmap (\a -> (a, b)) $
      successFn a


-- |
-- Tests the matched value on equality with the provided value.
{-# INLINABLE equals #-}
equals :: Eq a => a -> Matcher a ()
equals reference =
  Matcher $
  ReaderT $
  \input ->
    if input == reference
      then Success.Pure.success ()
      else Success.Pure.failure "The input doesn't equal the expected value"

-- |
-- Checks whether the matched value satisfies the provided predicate.
{-# INLINABLE satisfies #-}
satisfies :: (a -> Bool) -> Matcher a ()
satisfies predicate =
  Matcher $
  ReaderT $
  \input ->
    if predicate input
      then Success.Pure.success ()
      else Success.Pure.failure "The input doesn't satisfy the predicate"

-- |
-- Tries to convert the matched value to an output value,
-- with 'Either' encoding the success or failure of the conversion.
{-# INLINABLE converts #-}
converts :: (a -> Either Text b) -> Matcher a b
converts match =
  Matcher $
  ReaderT $
  either Success.Pure.failure Success.Pure.success . match

-- |
-- The matcher, which is always satisfied.
{-# INLINE whatever #-}
whatever :: Matcher a ()
whatever =
  Matcher $
  ReaderT $
  const $
  pure ()
