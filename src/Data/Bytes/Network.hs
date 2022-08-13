-- | This module serves as an alternative entry point to "Data.Bytes", for
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
module Data.Bytes.Network
  ( -- * Introduction
    -- $intro
    NetBytes (..),
    Size (..),
    Direction (..),

    -- * Basic Usage

    -- ** Construction
    -- $construction

    -- ** Unknown Size
    -- $size1
    SomeNetSize,
    -- $size2
    Sized (..),
    -- $size3

    -- ** Unknown Direction
    -- $direction1
    SomeNetDir,
    SomeNet,
    -- $direction2
    Directed (..),
    -- $direction3

    -- ** Elimination
    -- $elimination1
    Unwrapper (..),
    -- $elimination2

    -- ** Optics
    _MkNetBytes,
    _MkSomeNetSize,
    _MkSomeNetDir,
    _MkSomeNet,

    -- * Transformations

    -- ** Converting Units
    Conversion (..),

    -- ** Normalization
    Normalize (..),

    -- * Algebra
    -- $algebra
    module Numeric.Algebra,
    module Numeric.Data.NonZero,
    module Numeric.Literal.Integer,
    module Numeric.Literal.Rational,

    -- * Text

    -- ** Pretty Printing
    -- $pretty
    module Data.Bytes.Formatting,

    -- ** Parsing
    -- $parsing
    parse,

    -- * Reexports
    Default (def),
  )
where

import Data.Bytes.Class.Conversion (Conversion (..))
import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Class.Parser (parse)
import Data.Bytes.Class.Wrapper (Unwrapper (..))
import Data.Bytes.Formatting
import Data.Bytes.Network.Direction (Directed (..), Direction (..))
import Data.Bytes.Network.Internal
  ( NetBytes (..),
    SomeNet,
    SomeNetDir,
    SomeNetSize,
    _MkNetBytes,
    _MkSomeNet,
    _MkSomeNetDir,
    _MkSomeNetSize,
  )
import Data.Bytes.Size (Size (..), Sized (..))
import Numeric.Algebra
import Numeric.Data.NonZero
import Numeric.Literal.Integer
import Numeric.Literal.Rational

-- $intro
-- The main idea is to attach phantom labels to the numeric bytes, so we
-- can track the size and direction units. This allows us to safely
-- manipulate byte values without mixing up units, performing incorrect
-- conversions, etc.
--
-- The core types are a newtype wrapper 'NetBytes', the 'Size' units, and the
-- 'Direction' units:

-- $construction
-- There are several ways to construct a 'NetBytes' type.
--
-- 1. 'FromInteger'
--
--     >>> afromInteger 80 :: NetBytes Up M Int
--     MkNetBytes (MkBytes 80)
--
-- 2. Directly
--
--     >>> import Data.Bytes (Bytes (MkBytes))
--     >>> MkNetBytes (MkBytes 80) :: NetBytes Down M Int
--     MkNetBytes (MkBytes 80)
--
--     >>> -- using the @MkNetBytesP :: n -> NetBytes d s n@ pattern synonym
--     >>> MkNetBytesP 80 :: NetBytes Up K Int
--     MkNetBytes (MkBytes 80)
--
-- 3. Optics (@optics-core@)
--
--     >>> import Optics.Core (review, (%))
--     >>> import Data.Bytes (_MkBytes)
--     >>> (review _MkNetBytes (MkBytes 80)) :: NetBytes Up G Int -- Bytes -> NetBytes
--     MkNetBytes (MkBytes 80)
--
--     >>> (review (_MkNetBytes % _MkBytes) 70) :: NetBytes Up G Int -- n -> NetBytes
--     MkNetBytes (MkBytes 70)

-- $size1
-- We sometimes have to deal with unknown sizes at runtime, which presents
-- a problem. We handle this with the @'SomeNetSize'@ type, which existentially
-- quantifies the 'Size':

-- $size2
-- Fortunately, we do not have to directly use the constructor or singletons.
-- We can instead use the 'Sized' class.

-- $size3
-- Once again, we can use optics for this.
--
-- >>> import Optics.Core (review)
-- >>> review _MkSomeNetSize (MkNetBytesP 70 :: NetBytes Down G Int)
-- MkSomeNetSize SG (MkNetBytes (MkBytes 70))

-- $direction1
-- Like sizes, we can also handle unknown directions at runtime. The types
-- involved here are 'SomeNetDir' (hiding direction only) and 'SomeNet'
-- (hiding both direction and size).

-- $direction2
-- Analogous to the 'Sized' class, we have 'Directed'.

-- $direction3
-- Once again, we can use optics for this.
--
-- >>> import Optics.Core (review)
-- >>> let x = MkNetBytesP 70 :: NetBytes Down G Int
-- >>> review _MkSomeNetDir x
-- MkSomeNetDir SDown (MkNetBytes (MkBytes 70))
--
-- >>> review _MkSomeNet x
-- MkSomeNet SDown SG (MkNetBytes (MkBytes 70))

-- $elimination1
-- We provide the 'Unwrapper' class for conveniently unwrapping a type
-- to the underlying numeric value.

-- $elimination2
-- Optics can also be used, though they only unwrap one level at a time,
-- since we can freely compose them.
--
-- >>> import Optics.Core (view, (%))
-- >>> import Data.Bytes (_MkBytes)
-- >>> let x = MkNetBytesP 7 :: NetBytes Up G Int
-- >>> view (_MkNetBytes % _MkBytes) x
-- 7
--
-- >>> -- notice we have to convert the numeric value since the requested
-- >>> -- return type ('M') differs from the original ('G')
-- >>> let y = hideSize x :: SomeNetSize Up Int
-- >>> (view _MkSomeNetSize y) :: NetBytes Up M Int
-- MkNetBytes (MkBytes 7000)
--
-- >>> view (_MkSomeNetSize % (_MkNetBytes @M) % _MkBytes) y
-- 7000

-- $pretty
-- We provide several formatters for pretty-printing different byte types.
--
-- >>> import Data.Default (Default (def))
-- >>> let bf = MkFloatingFormatter (Just 2)
-- >>> let b = MkNetBytesP @Up @G @Float 203.301
-- >>> formatSizedDirected bf def def b
-- "203.30 gb up"

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
-- >>> import Numeric.Algebra (ASemigroup ((.+.)), AGroup ((.-.)))
-- >>> let mb1 = MkNetBytesP 20 :: NetBytes 'Down 'M Int
-- >>> let mb2 = MkNetBytesP 50 :: NetBytes 'Down 'M Int
-- >>> mb1 .+. mb2
-- MkNetBytes (MkBytes 70)
-- >>> mb1 .-. mb2
-- MkNetBytes (MkBytes (-30))
--
-- >>> let kb = MkNetBytesP 50 :: NetBytes 'Down 'K Int
-- >>> -- mb1 .+. kb -- This would be a type error
--
-- >>> let mbUp = MkNetBytesP 50 :: NetBytes 'Up 'M Int
-- >>> -- mb1 .+. mbUp -- This would be a type error
--
-- === Multiplication
-- >>> import Numeric.Algebra (MSemiSpace ((.*)))
-- >>> mb1 .* 10
-- MkNetBytes (MkBytes 200)
--
-- === Division
-- >>> import Numeric.Algebra (MSpace ((.%)))
-- >>> import Numeric.Data.NonZero (unsafeNonZero)
-- >>> mb1 .% (unsafeNonZero 10)
-- MkNetBytes (MkBytes 2)
--
-- One may wonder how the 'Numeric.Algebra.Additive.AGroup.AGroup' instance
-- for 'SomeNetSize' could possibly work. It is possible (indeed, expected)
-- that we could have two 'SomeNetSize's that have different underlying
-- 'NetBytes' types. To handle this, the 'SomeNetSize' instance will convert
-- both 'NetBytes' to a 'NetBytes' ''B' before adding/subtracting.
--
-- >>> let some1 = hideSize (MkNetBytesP 1000 :: NetBytes 'Down 'G Double)
-- >>> let some2 = hideSize (MkNetBytesP 500_000 :: NetBytes 'Down 'M Double)
-- >>> some1 .+. some2
-- MkSomeNetSize SB (MkNetBytes (MkBytes 1.5e12))
-- >>> some1 .-. some2
-- MkSomeNetSize SB (MkNetBytes (MkBytes 5.0e11))
--
-- This respects 'SomeNetSize'\'s equivalence-class base 'Eq'.

-- $parsing
-- We provide tools for parsing byte types from 'Data.Text.Text'. Parsing is
-- lenient in general. We support:
--
-- * Case-insensitivity.
-- * Optional leading\/internal\/trailing whitespace. Note the there must be
--   at least some whitespace between size and direction units.
-- * Flexible names.
--
-- __Examples__
--
-- >>> parse @(NetBytes Up M Int) "70"
-- Right (MkNetBytes (MkBytes 70))
--
-- >>> parse @(SomeNetSize Down Float) "100.45 kilobytes"
-- Right (MkSomeNetSize SK (MkNetBytes (MkBytes 100.45)))
--
-- >>> parse @(SomeNetSize Up Word) "2300G"
-- Right (MkSomeNetSize SG (MkNetBytes (MkBytes 2300)))
--
-- >>> parse @(SomeNetDir T Word) "2300 up"
-- Right (MkSomeNetDir SUp (MkNetBytes (MkBytes 2300)))
--
-- >>> parse @(SomeNetDir M Word) "2300D"
-- Right (MkSomeNetDir SDown (MkNetBytes (MkBytes 2300)))
--
-- >>> parse @(SomeNet Float) "5.5 tb Up"
-- Right (MkSomeNet SUp ST (MkNetBytes (MkBytes 5.5)))
--
-- >>> parse @(SomeNet Float) "5.5 megabytes DOWN"
-- Right (MkSomeNet SDown SM (MkNetBytes (MkBytes 5.5)))
