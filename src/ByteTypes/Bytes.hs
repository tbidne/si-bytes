-- | Module: ByteTypes.Bytes
--
-- This module serves as the main entry point for the library. It provides
-- the types and operations for typical usage and is usually the only import
-- required. The core concept is:
--
-- 1. Wrapping a numeric value representing bytes in a new type.
-- 2. Attaching phantom labels representing the units (e.g. K, M, ...).
--
-- This prevents mistakes, such as adding two different byte sizes or
-- converting between sizes incorrectly.
--
-- @since 0.1
module ByteTypes.Bytes
  ( -- * Types

    -- ** Units
    Size (..),

    -- ** Bytes
    Bytes (..),
    SomeSize,
    hideSize,

    -- * Transformations

    -- ** Pretty Printing
    -- $pretty
    PrettyPrint (..),

    -- ** Converting Units
    -- $convert
    Conversion (..),

    -- ** Normalization
    -- $normalize
    Normalize (..),

    -- * Algebra
    -- $algebra
  )
where

import ByteTypes.Class.Conversion (Conversion (..))
import ByteTypes.Class.Normalize (Normalize (..))
import ByteTypes.Class.PrettyPrint (PrettyPrint (..))
import ByteTypes.Data.Bytes (Bytes (..), SomeSize, hideSize)
import ByteTypes.Data.Size (Size (..))

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
-- >>> import Numeric.Algebra.Additive.AGroup (AGroup (..))
-- >>> import Numeric.Algebra.Semimodule (Semimodule (..))
-- >>> import Numeric.Algebra.VectorSpace (VectorSpace (..))
-- >>> import Numeric.Data.NonZero (unsafeNonZero)

-- $pretty
-- 'PrettyPrint', as the name suggests, is used for printing out bytes types
-- in a prettier manner than 'show' (i.e., no constructors, added units,
-- rounding).
--
-- >>> let b1 = MkBytes 50000 :: Bytes 'M Int
-- >>> let b2 = hideSize (MkBytes 20.40684 :: Bytes 'T Float)
-- >>> pretty b1
-- "50000 M"
-- >>> pretty b2
-- "20.41 T"

-- $normalize
-- This typeclass attempts to \"normalize\" a given 'Bytes' or 'SomeSize' such
-- that the result is between 1 and 1000, provided this is possible (i.e. we
-- cannot increase the max or decrease the min). Because the result type is
-- dependent on the value, 'normalize' necessarily existentially quantifies the
-- 'Size' i.e. returns 'SomeSize'.
--
-- >>> let bytes = MkBytes 5000 :: Bytes 'M Int
-- >>> normalize bytes
-- MkSomeSize SG (MkBytes {unBytes = 5})
--
-- >>> let bytes = hideSize (MkBytes 0.01 :: Bytes 'T Float)
-- >>> normalize bytes
-- MkSomeSize SG (MkBytes {unBytes = 10.0})

-- $convert
-- The 'Conversion' class allows one to transform 'Bytes' or 'SomeSize' to any
-- 'Size'. In the case of 'SomeSize', we can use this to fix the 'Size' and
-- \"undo\" the existential quantification.
--
-- >>> let bytes = MkBytes 50_000 :: Bytes 'M Int
-- >>> let gBytes = toG bytes
-- >>> :type gBytes
-- gBytes :: Bytes 'G Int
-- >>> gBytes
-- MkBytes {unBytes = 50}
--
-- >>> let bytes = hideSize (MkBytes 0.2 :: Bytes 'T Float)
-- >>> let mBytes = toM bytes
-- >>> :type mBytes
-- mBytes :: Bytes 'M Float
-- >>> mBytes
-- MkBytes {unBytes = 200000.0}

-- $algebra
--
-- The built-in 'Num' class is abandoned in favor of
-- [algebra-simple](https://github.com/tbidne/algebra-simple/)'s
-- algebraic hierarchy based on abstract algebra. This is motivated by a
-- desire to:
--
-- 1. Provide a consistent API.
-- 2. Avoid 'Num'\'s infelicities (e.g. nonsense multiplication,
--    dangerous 'fromInteger').
--
-- 'Bytes' and 'SomeSize' are both 'Numeric.Algebra.Additive.AGroup.AGroup's.
-- A 'Numeric.Algebra.Ring.Ring' instance is not provided because
-- multiplication is nonsensical:
--
-- \[
-- x \;\textrm{mb} \times y \;\textrm{mb} = xy \;\textrm{mb}^2.
-- \]
--
-- Fortunately, multiplying bytes by some kind of scalar is both useful /and/
-- has an easy interpretation: 'Bytes' forms a 'Numeric.Algebra.Module.Module'
-- over a 'Numeric.Algebra.Ring.Ring'
-- (resp. 'Numeric.Algebra.VectorSpace.VectorSpace' over a
-- 'Simple.Algebra.Field.Field'). This allows us to multiply a 'Bytes' or
-- 'SomeSize' by a scalar in a manner consistent with the above API.
--
-- == Examples
-- === Addition/Subtraction
-- >>> let mb1 = MkBytes 20 :: Bytes 'M Int
-- >>> let mb2 = MkBytes 50 :: Bytes 'M Int
-- >>> mb1 .+. mb2
-- MkBytes {unBytes = 70}
-- >>> mb1 .-. mb2
-- MkBytes {unBytes = -30}
--
-- >>> let kb = MkBytes 50 :: Bytes 'K Int
-- >>> -- mb1 .+. kb -- This would be a type error
--
-- === Multiplication
-- >>> mb1 .* 10
-- MkBytes {unBytes = 200}
--
-- === Division
-- >>> mb1 .% (unsafeNonZero 10)
-- MkBytes {unBytes = 2}
--
-- One may wonder how the 'Numeric.Algebra.Additive.AGroup.AGroup' instance
-- for 'SomeSize' could possibly work. It is possible (indeed, expected) that
-- we could have two 'SomeSize's that have different underlying 'Bytes' types.
-- To handle this, the 'SomeSize' instance will convert both 'Bytes' to a
-- 'Bytes' ''B' before adding/subtracting. The result will be normalized.
--
-- >>> let some1 = hideSize (MkBytes 1000 :: Bytes 'G Double)
-- >>> let some2 = hideSize (MkBytes 500_000 :: Bytes 'M Double)
-- >>> some1 .+. some2
-- MkSomeSize ST (MkBytes {unBytes = 1.5})
-- >>> some1 .-. some2
-- MkSomeSize SG (MkBytes {unBytes = 500.0})
--
-- This respects 'SomeSize'\'s equivalence-class based 'Eq'.
