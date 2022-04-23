-- | Module: ByteTypes.Network
--
-- This module serves as an alternative entry point to "ByteTypes.Bytes", for
-- when tracking uploaded vs. downloaded bytes is necessary. It provides the
-- types and operations for typical usage and is usually the only import
-- required. The core concept is:
--
-- 1. Wrapping a numeric value representing bytes in a new type.
-- 2. Attaching phantom labels representing the units (e.g. B, K, M, ...).
-- 3. Attaching phantom labels representing the direction (i.e. Down, Up).
--
-- This prevents mistakes, such as adding two different byte sizes/directions,
-- or converting between sizes incorrectly.
--
-- @since 0.1
module ByteTypes.Network
  ( -- * Types

    -- ** Units
    Size (..),
    Direction (..),

    -- ** Bytes
    NetBytes (..),
    SomeNetSize,
    hideNetSize,
    SomeNetDir,
    hideNetDir,
    SomeNet,
    hideNetSizeDir,

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
import ByteTypes.Data.Direction (Direction (..))
import ByteTypes.Data.Network.NetBytes.Internal
  ( NetBytes (..),
    SomeNetSize,
    hideNetSize,
  )
import ByteTypes.Data.Network.SomeNetDir.Internal
  ( SomeNet,
    SomeNetDir,
    hideNetDir,
    hideNetSizeDir,
  )
import ByteTypes.Data.Size (Size (..))

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup (ASemigroup (..))
-- >>> import Numeric.Algebra.Additive.AGroup (AGroup (..))
-- >>> import Numeric.Algebra.Semimodule (Semimodule (..))
-- >>> import Numeric.Algebra.VectorSpace (VectorSpace (..))
-- >>> import Numeric.Data.NonZero (unsafeNonZero)

-- $pretty
--
-- 'PrettyPrint', as the name suggests, is used for printing out bytes types
-- in a prettier manner than 'show' (i.e. no constructors, added units,
-- rounding).
--
-- >>> let b1 = MkNetBytesP 50000 :: NetBytes 'Down 'M Int
-- >>> let b2 = hideNetSize (MkNetBytesP 20.40684 :: NetBytes 'Down 'T Float)
-- >>> pretty b1
-- "50000 M Down"
-- >>> pretty b2
-- "20.41 T Down"

-- $normalize
-- This typeclass attempts to \"normalize\" a given bytes type such
-- that the result is between 1 and 1000, provided this is possible (i.e. we
-- cannot increase the max or decrease the min). Because the result type is
-- dependent on the value, 'normalize' necessarily existentially quantifies the
-- 'Size' i.e. returns 'SomeNetSize' (or 'SomeNet', in the case of 'SomeNetDir').
--
-- >>> let bytes = MkNetBytesP 5000 :: NetBytes 'Down 'M Int
-- >>> normalize bytes
-- MkSomeNetSize SG (MkNetBytesP {unNetBytesP = 5})
--
-- >>> let bytes = hideNetSize (MkNetBytesP 0.01 :: NetBytes 'Up 'T Float)
-- >>> normalize bytes
-- MkSomeNetSize SG (MkNetBytesP {unNetBytesP = 10.0})

-- $convert
-- The 'Conversion' class allows one to transform a bytes type to any
-- 'Size'. In the case of a type that has hidden the 'Size', we can use this
-- to fix the 'Size' and \"undo\" the existential quantification.
--
-- >>> let bytes = MkNetBytesP 50_000 :: NetBytes 'Down 'M Int
-- >>> let gBytes = toG bytes
-- >>> :type gBytes
-- gBytes :: NetBytes 'Down 'G Int
-- >>> gBytes
-- MkNetBytesP {unNetBytesP = 50}
--
-- >>> let bytes = hideNetSize (MkNetBytesP 0.2 :: NetBytes 'Up 'T Float)
-- >>> let mBytes = toM bytes
-- >>> :type mBytes
-- mBytes :: NetBytes 'Up 'M Float
-- >>> mBytes
-- MkNetBytesP {unNetBytesP = 200000.0}

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
-- 'NetBytes' and 'SomeNetSize' are both
-- 'Numeric.Algebra.Additive.AGroup.AGroup's. A 'Numeric.Algebra.Ring.Ring'
-- instance is not provided because multiplication is nonsensical:
--
-- \[
-- x \;\textrm{mb} \times y \;\textrm{mb} = xy \;\textrm{mb}^2.
-- \]
--
-- Fortunately, multiplying bytes by some kind of scalar is both useful /and/
-- has an easy interpretation: 'NetBytes' forms a
-- 'Numeric.Algebra.Module.Module' over a 'Numeric.Algebra.Ring.Ring'
-- (resp. 'Numeric.Algebra.VectorSpace.VectorSpace' over a
-- 'Numeric.Algebra.Field.Field'). This allows us to multiply a 'NetBytes' or
-- 'SomeNetSize' by a scalar in a manner consistent with the above API.
--
-- == Examples
-- === Addition/Subtraction
-- >>> let mb1 = MkNetBytesP 20 :: NetBytes 'Down 'M Int
-- >>> let mb2 = MkNetBytesP 50 :: NetBytes 'Down 'M Int
-- >>> mb1 .+. mb2
-- MkNetBytesP {unNetBytesP = 70}
-- >>> mb1 .-. mb2
-- MkNetBytesP {unNetBytesP = (-30)}
--
-- >>> let kb = MkNetBytesP 50 :: NetBytes 'Down 'K Int
-- >>> -- mb1 .+. kb -- This would be a type error
--
-- >>> let mbUp = MkNetBytesP 50 :: NetBytes 'Up 'M Int
-- >>> -- mb1 .+. mbUp -- This would be a type error
--
-- === Multiplication
-- >>> mb1 .* 10
-- MkNetBytesP {unNetBytesP = 200}
--
-- === Division
-- >>> mb1 .% (unsafeNonZero 10)
-- MkNetBytesP {unNetBytesP = 2}
--
-- One may wonder how the 'Numeric.Algebra.Additive.AGroup.AGroup' instance
-- for 'SomeNetSize' could possibly work. It is possible (indeed, expected)
-- that we could have two 'SomeNetSize's that have different underlying
-- 'NetBytes' types. To handle this, the 'SomeNetSize' instance will convert
-- both 'NetBytes' to a 'NetBytes' ''B' before adding/subtracting. The result
-- will be normalized.
--
-- >>> let some1 = hideNetSize (MkNetBytesP 1000 :: NetBytes 'Down 'G Double)
-- >>> let some2 = hideNetSize (MkNetBytesP 500_000 :: NetBytes 'Down 'M Double)
-- >>> some1 .+. some2
-- MkSomeNetSize ST (MkNetBytesP {unNetBytesP = 1.5})
-- >>> some1 .-. some2
-- MkSomeNetSize SG (MkNetBytesP {unNetBytesP = 500.0})
