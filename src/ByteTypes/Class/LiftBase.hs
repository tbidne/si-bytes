-- | This module provides the 'LiftBase' typeclass.
module ByteTypes.Class.LiftBase
  ( LiftBase (..),
  )
where

-- | LiftBase provides lifts from a base type. This is an alternative for the
-- @liftAN@ functionality from 'Control.Applicative', for when we cannot make
-- our types an 'Applicative'. For example:
--
-- @
--   data Tag = One | Two
--
--   data Foo t b :: Tag -> Type -> Type where
--     FooOne :: b -> Foo One b
--     FooTwo :: b -> Foo Two b
--
--   instance Applicative (Foo t) where
--     pure x = ???
-- @
--
-- The problem in this case is the @t@ is universally quantified, so we
-- cannot know which constructor to choose. With dependent types and
-- relevant @t@ we could imagine something like:
--
-- @
--   instance Applicative (Foo t) where
--     pure x = mkFoo t x
--       where
--         mkFoo :: foreach (t :: Tag) -> a -> Foo t a
--         mkFoo One x = FooOne x
--         mkFoo Two x = FooTwo x
-- @
--
-- Alas, for now we are stuck doing things the hard way. Another downside
-- of this approach is we cannot abstract over 'LiftBase', since the
-- exact functionality we need to build generic @liftAN@ functions is
-- trapped in 'Applicative'. Thus this typeclass is only used for
-- overloading symbols (e.g. 'liftB') for different types, which provides
-- some value, but not as much as canonical typeclasses, e.g., 'Functor'.
class LiftBase a where
  type Base a
  liftB :: (Base a -> Base a) -> a -> a
  liftB2 :: (Base a -> Base a -> Base a) -> a -> a -> a
  liftB3 :: (Base a -> Base a -> Base a -> Base a) -> a -> a -> a -> a
