-- | This module serves as the main entry point for the library. It provides
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
module Data.Bytes
  ( -- * Introduction
    -- $intro
    Bytes (..),
    Size (..),

    -- * Basic Usage

    -- ** Construction
    -- $construction

    -- ** Unknown Size
    -- $size1
    SomeSize,
    -- $size2
    Sized (..),
    -- $size3

    -- ** Elimination
    -- $elimination1
    Unwrapper (..),
    -- $elimination2

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
    Parser,
    parse,

    -- * Optics

    -- ** Core
    _MkBytes,
    _MkSomeSize,

    -- ** Size
    _B,
    _K,
    _M,
    _G,
    _T,
    _P,
    _E,
    _Z,
    _Y,

    -- * Reexports
    Default (def),
  )
where

import Data.Bytes.Class.Conversion (Conversion (..))
import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Class.Parser (Parser, parse)
import Data.Bytes.Class.Wrapper (Unwrapper (..))
import Data.Bytes.Formatting
import Data.Bytes.Internal
  ( Bytes (..),
    SomeSize,
    _MkBytes,
    _MkSomeSize,
  )
import Data.Bytes.Size
  ( Size (..),
    Sized (..),
    _B,
    _E,
    _G,
    _K,
    _M,
    _P,
    _T,
    _Y,
    _Z,
  )
import Numeric.Algebra
import Numeric.Data.NonZero
import Numeric.Literal.Integer
import Numeric.Literal.Rational

-- $intro
-- The main idea is to attach phantom labels to the numeric bytes, so we
-- can track the size units. This allows us to safely manipulate byte values
-- without mixing up units, performing incorrect conversions, etc.
--
-- The core types are a newtype wrapper 'Bytes' and the 'Size' units:

-- $construction
-- There are several ways to construct a 'Bytes' type.
--
-- 1. 'FromInteger'
--
--     >>> afromInteger 80 :: Bytes M Int
--     MkBytes 80
--
-- 2. Directly
--
--     >>> MkBytes 80 :: Bytes M Int
--     MkBytes 80
--
-- 3. Optics (@optics-core@)
--
--     >>> import Optics.Core (review)
--     >>> (review _MkBytes 70) :: Bytes G Int
--     MkBytes 70

-- $size1
-- We sometimes have to deal with unknown sizes at runtime, which presents
-- a problem. We handle this with the @'SomeSize'@ type, which existentially
-- quantifies the 'Size':

-- $size2
-- Fortunately, we do not have to directly use the constructor or singletons.
-- We can instead use the 'Sized' class.

-- $size3
-- Once again, we can use optics for this.
--
-- >>> import Optics.Core (review)
-- >>> review _MkSomeSize (MkBytes 70 :: Bytes G Int)
-- MkSomeSize SG (MkBytes 70)

-- $elimination1
-- We provide the 'Unwrapper' class for conveniently unwrapping a type
-- to the underlying numeric value.

-- $elimination2
-- Optics can also be used, though they only unwrap one level at a time,
-- since we can freely compose them.
--
-- >>> import Optics.Core (view, (%))
-- >>> let x = MkBytes 7 :: Bytes G Int
-- >>> view _MkBytes x
-- 7
--
-- >>> -- notice we have to convert the numeric value since the requested
-- >>> -- return type ('M') differs from the original ('G')
-- >>> let y = hideSize x :: SomeSize Int
-- >>> (view _MkSomeSize y) :: Bytes M Int
-- MkBytes 7000
--
-- >>> view (_MkSomeSize % (_MkBytes @M)) y
-- 7000

-- $pretty
-- We provide several formatters for pretty-printing different byte types.
--
-- >>> import Data.Default (Default (def))
-- >>> let bf = MkFloatingFormatter (Just 2)
-- >>> let b = MkBytes @G @Float 20.248
-- >>> formatSized bf def b
-- "20.25 gb"

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
-- >>> import Numeric.Algebra (ASemigroup ((.+.)), AGroup ((.-.)))
-- >>> let mb1 = MkBytes 20 :: Bytes 'M Int
-- >>> let mb2 = MkBytes 50 :: Bytes 'M Int
-- >>> mb1 .+. mb2
-- MkBytes 70
-- >>> mb1 .-. mb2
-- MkBytes (-30)
--
-- >>> let kb = MkBytes 50 :: Bytes 'K Int
-- >>> -- mb1 .+. kb -- This would be a type error
--
-- === Multiplication
-- >>> import Numeric.Algebra (MSemiSpace ((.*)))
-- >>> mb1 .* 10
-- MkBytes 200
--
-- === Division
-- >>> import Numeric.Algebra (MSpace ((.%)))
-- >>> import Numeric.Data.NonZero (unsafeNonZero)
-- >>> mb1 .% (unsafeNonZero 10)
-- MkBytes 2
--
-- One may wonder how the 'Numeric.Algebra.Additive.AGroup.AGroup' instance
-- for 'SomeSize' could possibly work. It is possible (indeed, expected) that
-- we could have two 'SomeSize's that have different underlying 'Bytes' types.
-- To handle this, the 'SomeSize' instance will convert both 'Bytes' to a
-- 'Bytes' ''B' before adding/subtracting.
--
-- >>> let some1 = hideSize (MkBytes 1000 :: Bytes 'G Double)
-- >>> let some2 = hideSize (MkBytes 500_000 :: Bytes 'M Double)
-- >>> some1 .+. some2
-- MkSomeSize SB (MkBytes 1.5e12)
-- >>> some1 .-. some2
-- MkSomeSize SB (MkBytes 5.0e11)
--
-- This respects 'SomeSize'\'s equivalence-class based 'Eq'.
