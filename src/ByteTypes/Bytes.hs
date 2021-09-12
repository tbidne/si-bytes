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

    -- * Convenient Mathematical Operations
    -- $othermath
    module ByteTypes.Class.Math.Literal,
    module ByteTypes.Class.Math.Scalar.Ord,
    module ByteTypes.Class.Math.Scalar.Num,
  )
where

import ByteTypes.Class.Conversion
import ByteTypes.Class.Math.Algebra.Field
import ByteTypes.Class.Math.Algebra.Group
import ByteTypes.Class.Math.Algebra.Module
import ByteTypes.Class.Math.Algebra.Ring
import ByteTypes.Class.Math.Algebra.VectorSpace
import ByteTypes.Class.Math.Literal
import ByteTypes.Class.Math.Scalar.Num
import ByteTypes.Class.Math.Scalar.Ord
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
-- Most of the time the 'Bytes' type should be preferred.
-- 'SomeSize' is useful when we do not know the size at compile-time
-- (e.g. parsing the output of @ls -lh@ at runtime), or when we use
-- 'normalize'.
--
-- == Modules

-- $transformations
--
-- == Normalization
--
-- The primary transformation of interest is 'Normalize'.
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
-- >>> let bytes = MkSomeSize ST (MkBytes 0.01 :: Bytes 'T Float)
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
-- @
--
-- >>> let bytes = MkBytes 50_000 :: Bytes 'M Int
-- >>> toG bytes
-- MkBytes {unBytes = 50}
--
-- >>> let bytes = MkSomeSize ST (MkBytes 0.2 :: Bytes 'T Float)
-- >>> toM bytes
-- MkBytes {unBytes = 200000.0}
--
-- == Pretty Printing
--
-- 'PrettyPrint' is, as the num suggests, used for printing out bytes types
-- in a prettier manner than 'show' (i.e., no constructors, added units,
-- rounding).
--
-- @
-- class 'PrettyPrint' a where
--   'pretty' :: a -> 'String'
-- @
--
-- >>> let bytes = MkBytes 50000 :: Bytes 'M Int
-- >>> pretty bytes
-- "50000 M"
--
-- >>> let bytes = MkSomeSize ST (MkBytes 20.40684 :: Bytes 'T Float)
-- >>> pretty bytes
-- "20.41 T"
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
--     class ('Group' m, 'Ring' r) => 'Module' m r where
--       '(.*)' :: m -> r -> m
--       '(*.)' :: r -> m -> m
--     @
--
-- 5. 'VectorSpace'
--
--     @
--     class ('Field' k, 'Module' v k) => 'VectorSpace' v k where
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
-- MkBytes {unBytes = 70}
--
-- >>> mb1 .-. mb2
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
-- >>> mb1 .* (10 :: Int)
-- MkBytes {unBytes = 200}
--
-- === Division
-- >>> mb1 .% (unsafeNonZero 10 :: NonZero Int)
-- MkBytes {unBytes = 2}
--
-- One may wonder how the 'Group' instance for 'SomeSize' could possibly
-- work. It is possible (indeed, expected) that we could have two 'SomeSize's
-- that have different underlying 'Bytes' types. To handle this, the
-- 'SomeSize' instance will convert both 'Bytes' to a 'Bytes' ''B' before
-- adding/subtracting. The result will be normalized.
--
-- >>> let some1 = MkSomeSize SG (MkBytes 1000) :: SomeSize Double
-- >>> let some2 = MkSomeSize SM (MkBytes 500_000) :: SomeSize Double
-- >>> some1 .+. some2
-- MkSomeSize ST (MkBytes {unBytes = 1.5})
--
-- >>> some1 .-. some2
-- MkSomeSize SG (MkBytes {unBytes = 500.0})
--
-- In fact, this is how 'SomeSize'\'s 'Eq' and 'Ord' classes work. They
-- convert both arguments to ''B' first. This establishes an equivalence
-- class determined by the numeric value /and/ the size. For example,
--
-- >>> let some1 = MkSomeSize SG (MkBytes 7 :: Bytes 'G Int)
-- >>> let some2 = MkSomeSize SM (MkBytes 7000 :: Bytes 'M Int)
-- >>> some1 == some2
-- True
--
-- >>> let some3 = MkSomeSize ST (MkBytes 2 :: Bytes 'T Int)
-- >>> some1 < some3
-- True
--
-- >>> some2 < some3
-- True
--
-- == Modules

-- $othermath
--
-- Even with the algebraic classes above, we are still short on replacing some
-- of 'Num'\'s nice functionality: using numeric literals. With 'Num' this is
-- achieved through 'fromInteger'; with 'Fractional', 'fromRational'.
--
-- To replicate this, we have provided typeclasses that allow us to compare
-- our types to some kind of scalar. 'ScalarEq' and 'ScalarOrd' replace
-- 'Eq' and 'Ord', respectively. 'ScalarNum' allows us to add and subtract
-- numeric literals
--
-- @
-- class 'ScalarEq' a where
--   '(.=)' :: a -> 'ByteTypes.Class.Math.Scalar.Scalar.Scalar' a -> 'Bool'
--   '(=.)' :: 'ByteTypes.Class.Math.Scalar.Scalar.Scalar' a -> a -> 'Bool'
--   '(./=)' :: a -> 'ByteTypes.Class.Math.Scalar.Scalar.Scalar' a -> 'Bool'
--   '(/=.)' :: 'ByteTypes.Class.Math.Scalar.Scalar.Scalar' a -> a -> 'Bool'
-- @
--
-- >>> let bytes = MkBytes 10 :: Bytes 'G Int
-- >>> bytes .= 10
-- True
--
-- >>> bytes ./= 10
-- False
--
-- @
-- class 'ScalarEq' a => 'ScalarOrd' a where
--   -- LHS
--   'lcompare' :: a -> 'ByteTypes.Class.Math.Scalar.Scalar.Scalar' a -> 'Ordering'
--   '(.<)' :: a -> 'ByteTypes.Class.Math.Scalar.Scalar.Scalar' a -> 'Bool'
--   '(.<=)' :: a -> 'ByteTypes.Class.Math.Scalar.Scalar.Scalar' a -> 'Bool'
--   '(.>)' :: a -> 'ByteTypes.Class.Math.Scalar.Scalar.Scalar' a -> 'Bool'
--   '(.>=)' :: a -> 'ByteTypes.Class.Math.Scalar.Scalar.Scalar' a -> 'Bool'
--
--   -- RHS
--   'rcompare' :: 'ByteTypes.Class.Math.Scalar.Scalar.Scalar' a -> a -> 'Ordering'
--   '(<.)' :: 'ByteTypes.Class.Math.Scalar.Scalar.Scalar' a -> a -> 'Bool'
--   '(<=.)' :: 'ByteTypes.Class.Math.Scalar.Scalar.Scalar' a -> a -> 'Bool'
--   '(>.)' :: 'ByteTypes.Class.Math.Scalar.Scalar.Scalar' a -> a -> 'Bool'
--   '(>=.)' :: 'ByteTypes.Class.Math.Scalar.Scalar.Scalar' a -> a -> 'Bool'
-- @
--
-- >>> let bytes = MkBytes 400 :: Bytes 'K Int
-- >>> bytes .< 400
-- False
--
-- >>> 800 >. bytes
-- True
--
-- >>> lcompare bytes 500
-- LT
--
-- @
-- class 'ScalarNum' m where
--   '(.+)' :: m -> 'ByteTypes.Class.Math.Scalar.Scalar.Scalar' m -> m
--   '(.-)' :: m -> 'ByteTypes.Class.Math.Scalar.Scalar.Scalar' m -> m
--   '(+.)' :: 'ByteTypes.Class.Math.Scalar.Scalar.Scalar' m -> m -> m
-- @
--
-- >>> let bytes = MkBytes 400 :: Bytes 'K Int
-- >>> bytes .+ 200
-- MkBytes {unBytes = 600}
--
-- >>> bytes .- 300
-- MkBytes {unBytes = 100}
--
-- 'NumLiteral' gives us the 'fromLit' function, which we can use to transform
-- 'Integer' literals to the expected scalar.
--
-- @
-- class 'NumLiteral' a where
--   'fromLit' :: 'Integer' -> a
-- @
--
-- This is generally only necessary when writing functions polymorphic over
-- some numeric type @n@, and we don't have a 'Num' constraint.
--
--
-- >>> let times1000 :: (NumLiteral n, Ring n) => n -> n; times1000 x = x .*. fromLit (1000 :: Integer)
-- >>> times1000 (5 :: Int)
-- 5000
--
-- == Modules
