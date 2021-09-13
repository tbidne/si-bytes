-- |
--
-- Module: ByteTypes.Bytes
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
module ByteTypes.Bytes
  ( -- * Types
    -- $types
    module ByteTypes.Data.Size,
    module ByteTypes.Data.Bytes,

    -- * Byte Transformations
    -- $transformations
    module ByteTypes.Class.Conversion,
    module ByteTypes.Class.Normalize,
    module ByteTypes.Class.PrettyPrint,

    -- * Algebraic Functions
    -- $algebra
    module ByteTypes.Class.Math.Algebra.Group,
    module ByteTypes.Class.Math.Algebra.Ring,
    module ByteTypes.Class.Math.Algebra.Field,
    module ByteTypes.Class.Math.Algebra.Module,
    module ByteTypes.Class.Math.Algebra.VectorSpace,
  )
where

import ByteTypes.Class.Conversion
import ByteTypes.Class.Math.Algebra.Field
import ByteTypes.Class.Math.Algebra.Group
import ByteTypes.Class.Math.Algebra.Module
import ByteTypes.Class.Math.Algebra.Ring
import ByteTypes.Class.Math.Algebra.VectorSpace
import ByteTypes.Class.Normalize
import ByteTypes.Class.PrettyPrint
import ByteTypes.Data.Bytes
import ByteTypes.Data.Size

-- $types
-- The are three main types exported in this module:
--
-- 1. 'Size': These are the units that can be attached to a numeric bytes.
--
--     @
--     data 'Size' = 'B' | 'K' | 'M' | 'G' | 'T' | 'P' | 'E' | 'Z' | 'Y'
--     @
--
-- 2. 'Bytes': The core type, wraps a numeric value and includes a 'Size'
--    phantom type.
--
--     @
--     type 'Bytes' :: 'Size' -> 'Data.Kind.Type' -> 'Data.Kind.Type'
--     newtype 'Bytes' s n = 'MkBytes' { 'unBytes' :: n }
--     @
--
-- 3. 'SomeSize': A GADT that wraps 'Bytes' and existentially quantifies the
-- 'Size'.
--
--     @
--     type 'SomeSize' :: 'Data.Kind.Type' -> 'Data.Kind.Type'
--     data 'SomeSize' n
--
--     'hideSize' :: forall s n. 'SingSize' s => 'Bytes' s n -> 'SomeSize' n
--     @
--
-- 'SomeSize' defines an equivalence class that takes units into account.
-- For instance,
--
-- >>> let some1 = hideSize (MkBytes 7 :: Bytes 'G Int)
-- >>> let some2 = hideSize (MkBytes 7000 :: Bytes 'M Int)
-- >>> let some3 = hideSize (MkBytes 2 :: Bytes 'T Int)
-- >>> some1 == some2
-- >>> some2 < some3
-- True
-- True
--
-- Most of the time the 'Bytes' type should be preferred. 'SomeSize' is useful
-- when we do not know the 'Size' at compile-time (e.g. parsing the output of
-- @ls -lh@ at runtime), or when we use 'normalize'. In general, once we wrap
-- a 'Bytes' in a 'SomeSize' we should think of the 'Size' as being \"lost\",
-- unless we explicitly recover it with a 'Conversion' function. This is
-- necessary for 'SomeSize'\'s algebraic instances (e.g. 'Eq', 'Group') to be
-- lawful, as functions that inspect the underlying size or numeric value
-- can break the equivalence class. Nevertheless, 'SomeSize'\'s internal
-- representation can be used to recover the size, in case this is
-- needed (see: "ByteTypes.Data.Bytes.Internal").
--
-- == Modules

-- $transformations
--
-- == Pretty Printing
--
-- 'PrettyPrint', as the name suggests, is used for printing out bytes types
-- in a prettier manner than 'show' (i.e., no constructors, added units,
-- rounding).
--
-- @
-- class 'PrettyPrint' a where
--   'pretty' :: a -> 'String'
-- @
--
-- >>> let b1 = MkBytes 50000 :: Bytes 'M Int
-- >>> let b2 = hideSize (MkBytes 20.40684 :: Bytes 'T Float)
-- >>> b1
-- >>> b2
-- MkBytes {unBytes = 50000}
-- MkSomeSize ST (MkBytes {unBytes = 20.40684})
--
-- == Normalization
--
-- @
-- class 'Normalize' a where
--   type 'Norm' a
--   'normalize' :: a -> 'Norm' a
-- @
--
-- This typeclass attempts to \"normalize\" a given 'Bytes' or 'SomeSize' such
-- that the result is between 1 and 1000, provided this is possible (i.e. we
-- cannot increase the max or decrease the min). Because the result type is
-- dependent on the value, 'normalize' necessarily existentially quantifies the
-- 'Size', i.e., returns 'SomeSize'.
--
-- >>> let bytes = MkBytes 5000 :: Bytes 'M Int
-- >>> normalize bytes
-- MkSomeSize SG (MkBytes {unBytes = 5})
--
-- >>> let bytes = hideSize (MkBytes 0.01 :: Bytes 'T Float)
-- >>> normalize bytes
-- MkSomeSize SG (MkBytes {unBytes = 10.0})
--
-- == Conversion
--
-- The 'Conversion' class allows one to transform 'Bytes' or 'SomeSize' to any
-- 'Size'. In the case of 'SomeSize', we can use this to fix the 'Size' and
-- \"undo\" the existential quantification.
--
-- @
-- class 'Conversion' a where
--   type 'Converted' (b :: 'Size') a = r | r -> b
--
--   'toB' :: a -> 'Converted' ''B' a
--   'toK' :: a -> 'Converted' ''K' a
--   'toM' :: a -> 'Converted' ''M' a
--   'toG' :: a -> 'Converted' ''G' a
--   'toT' :: a -> 'Converted' ''T' a
--   'toP' :: a -> 'Converted' ''P' a
--   'toE' :: a -> 'Converted' ''E' a
--   'toZ' :: a -> 'Converted' ''Z' a
--   'toY' :: a -> 'Converted' ''Y' a
-- @
--
-- >>> let bytes = MkBytes 50_000 :: Bytes 'M Int
-- >>> let gBytes = toG bytes
-- >>> :type gBytes
-- >>> gBytes
-- gBytes :: Bytes 'G Int
-- MkBytes {unBytes = 50}
--
-- >>> let bytes = hideSize (MkBytes 0.2 :: Bytes 'T Float)
-- >>> let mBytes = toM bytes
-- >>> :type mBytes
-- >>> mBytes
-- mBytes :: Bytes 'M Float
-- MkBytes {unBytes = 200000.0}
--
-- == Modules

-- $algebra
--
-- The built-in 'Num' class is abandoned in favor of a custom algebraic
-- hierarchy. This is motivated by a desire to:
--
-- 1. Provide a consistent API.
-- 2. Avoid 'Num'\'s infelicities (e.g. nonsense multiplication,
--    dangerous 'fromInteger').
--
-- The hierarchy includes:
--
-- 1. 'Group'
--
--     @
--     class 'Eq' g => 'Group' g where
--       '(.+.)' :: g -> g -> g
--       '(.-.)' :: g -> g -> g
--       'gid' :: g
--       'ginv' :: g -> g
--       'gabs' :: g -> g
--     @
--
-- 2. 'Ring'
--
--     @
--     class 'Group' r => 'Ring' r where
--       '(.*.)' :: r -> r -> r
--       'rid' :: r
--     @
--
-- 3. 'Field'
--
--     @
--     class 'Ring' f => 'Field' f where
--       'finv' :: 'NonZero' f -> 'NonZero' f
--       '(.%.)' :: f -> 'NonZero' f -> f
--     @
--
-- 4. 'Module'
--
--     @
--     class ('Group' m, 'Ring' r) => 'Module' m r | m -> r where
--       '(.*)' :: m -> r -> m
--       '(*.)' :: r -> m -> m
--     @
--
-- 5. 'VectorSpace'
--
--     @
--     class ('Field' k, 'Module' v k) => 'VectorSpace' v k | v -> k where
--       '(.%)' :: v -> 'NonZero' k -> v
--     @
--
-- Built-in numeric types (e.g. 'Integer', 'Double', 'Rational') have been
-- given 'Group', 'Ring', and 'Field' instances with the obvious addition and
-- multiplication. 'Integral' and 'Floating' do not follow the field
-- laws w.r.t. division, but this is a common tradeoff. We take the stance
-- that it is better to provide an expected, useful notion of division
-- (albeit one with known limitations) than none.
--
-- 'Bytes' and 'SomeSize' are both 'Group's compatible with the above.
-- A 'Ring' instance is not provided because multiplication is nonsensical:
--
-- \[
-- x \;\textrm{mb} \times y \;\textrm{mb} = xy \;\textrm{mb}^2.
-- \]
--
-- Fortunately, multiplying bytes by some kind of scalar is both useful /and/
-- has an easy interpretation: Bytes forms a 'Module' over a 'Ring'
-- (resp. 'VectorSpace' over a 'Field'). This allows us to multiply a 'Bytes'
-- or 'SomeSize' by a scalar in a manner consistent with the above API.
--
-- == Examples
-- === Addition/Subtraction
-- >>> let mb1 = MkBytes 20 :: Bytes 'M Int
-- >>> let mb2 = MkBytes 50 :: Bytes 'M Int
-- >>> mb1 .+. mb2
-- >>> mb1 .-. mb2
-- MkBytes {unBytes = 70}
-- MkBytes {unBytes = -30}
--
-- >>> -- Type error!
-- >>> let kb = MkBytes 50 :: Bytes 'K Int
-- >>> mb1 .+. kb
-- Couldn't match type ‘'K’ with ‘'M’
-- Expected type: Bytes 'M Int
--   Actual type: Bytes 'K Int
--
-- === Multiplication
-- >>> mb1 .* 10
-- MkBytes {unBytes = 200}
--
-- === Division
-- >>> mb1 .% (unsafeNonZero 10)
-- MkBytes {unBytes = 2}
--
-- One may wonder how the 'Group' instance for 'SomeSize' could possibly
-- work. It is possible (indeed, expected) that we could have two 'SomeSize's
-- that have different underlying 'Bytes' types. To handle this, the
-- 'SomeSize' instance will convert both 'Bytes' to a 'Bytes' ''B' before
-- adding/subtracting. The result will be normalized.
--
-- >>> let some1 = hideSize (MkBytes 1000 :: Bytes 'G Double)
-- >>> let some2 = hideSize (MkBytes 500_000 :: Bytes 'M Double)
-- >>> some1 .+. some2
-- >>> some1 .-. some2
-- MkSomeSize ST (MkBytes {unBytes = 1.5})
-- MkSomeSize SG (MkBytes {unBytes = 500.0})
--
-- This respects 'SomeSize'\'s equivalence-class based 'Eq'.
--
-- == Modules
