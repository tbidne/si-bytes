-- | Provides the 'VectorSpace' typeclass.
module ByteTypes.Class.Math.Algebra.VectorSpace
  ( VectorSpace (..),
  )
where

import ByteTypes.Class.Math.Algebra.Field (Field (..))
import ByteTypes.Class.Math.Algebra.Module (Module (..))

-- | Defines an algebraic vector space over a field. Ideally, this class does
-- not need to include any functions. The only difference between a 'Module'
-- and a 'VectorSpace' is that the former is over a
-- 'ByteTypes.Class.Math.Algebra.Ring' and the latter a 'Field', so we get
-- scalar \"division\" for free simply by reusing multiplicative
-- inverses. Sadly, this cannot be trusted in the real world, so we provide
-- the above as a default implementation that can be overridden. This default
-- is sensible for:
--
-- 1. Floating points. It is generally not true that @x \/ d == x * (1 \/ d)@
-- for floats, but this is reasonably close, and all bets are off when
-- comparing floats for equality anyway.
-- 2. 'GHC.Real.Ratio' over an 'Integral' type. In this case the above law
-- is actually respected.
--
-- Conspicuously absent is integer types. '(.%)' must be overridden for
-- integers! Integer division truncates, so @1 \/ d == 0@ is always true
-- for @d > 1@. Consequently this typeclass is useless for integers unless
-- the function is overridden (presumably with 'div'). Thus, if it's possible
-- the type providing a 'VectorSpace' instance is ever used with integers,
-- it should explicitly override this function (presumably using a sensible
-- instance for '(.%.)').
class (Field k, Module v k) => VectorSpace v k where
  (.%) :: v -> k -> v
  x .% k = x .* finv k

infixl 7 .%
