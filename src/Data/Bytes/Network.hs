{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

{- ORMOLU_DISABLE -}

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

    -- *** Sized
    -- $size2
    Sized (..),

    -- *** Optics
    -- $size3

    -- ** Unknown Direction
    -- $direction1
    SomeNetDir,
    SomeNet,
    -- $direction2
    Directed (..),
    -- $direction3

    -- ** Elimination

    -- *** RawNumeric
    -- $elimination1
    RawNumeric (..),

    -- *** HasField
    -- $elimination2

    -- *** Optics
    -- $elimination3

    -- * Transformations

    -- ** Converting Units
    Conversion (..),

#if MIN_VERSION_base(4, 20, 0)
    Conv.convert,
#endif

    -- ** Normalization
    Normalize (..),

    -- * Algebra
    -- $algebra
    module Numeric.Algebra,
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
    _MkNetBytes,
    _MkSomeNetSize,
    _MkSomeNetDir,
    _MkSomeNet,

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

    -- ** Direction
    _Down,
    _Up,

    -- * Reexports
    Default (def),
  )
where

{- ORMOLU_ENABLE -}

import Data.Bytes.Class.Conversion (Conversion (Converted, convert_))
#if MIN_VERSION_base(4, 20, 0)
import Data.Bytes.Class.Conversion qualified as Conv
#endif
import Data.Bytes.Class.Normalize (Normalize (Norm, normalize))
import Data.Bytes.Class.Parser (Parser, parse)
import Data.Bytes.Class.RawNumeric (RawNumeric (Raw, toRaw))
import Data.Bytes.Formatting
import Data.Bytes.Network.Direction
  ( Directed (HideDirection, directionOf, hideDirection),
    Direction (Down, Up),
    _Down,
    _Up,
  )
import Data.Bytes.Network.Internal
  ( NetBytes (MkNetBytes, MkNetBytesP),
    SomeNet,
    SomeNetDir,
    SomeNetSize,
    _MkNetBytes,
    _MkSomeNet,
    _MkSomeNetDir,
    _MkSomeNetSize,
  )
import Data.Bytes.Size
  ( Size (B, E, G, K, M, P, T, Y, Z),
    Sized (HideSize, hideSize, sizeOf),
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
--     >>> -- 1. Using _MkNetBytes to construct a NetBytes from a Bytes
--     >>> import Optics.Core (review, (%))
--     >>> import Data.Bytes (_MkBytes)
--     >>> (review _MkNetBytes (MkBytes 80)) :: NetBytes Up G Int -- Bytes -> NetBytes
--     MkNetBytes (MkBytes 80)
--
--     >>> -- 2. Using _MkNetBytes and _MkBytes to construct a NetBytes from a
--     >>> --    numeric value.
--     >>> (review (_MkNetBytes % _MkBytes) 70) :: NetBytes Up G Int -- n -> NetBytes
--     MkNetBytes (MkBytes 70)
--
--     >>> -- 3. Using #unNetBytes to construct a NetBytes from a numeric value.
--     >>> (review #unNetBytes 70) :: NetBytes Up G Int -- n -> NetBytes
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
-- We provide the 'RawNumeric' class for conveniently unwrapping a type
-- to the underlying numeric value.

-- $elimination2
-- We can use 'GHC.Records.HasField' for this too.
--
-- >>> -- {-# LANGUAGE OverloadedRecordDot #-}
-- >>> let x1 = MkNetBytesP 7 :: NetBytes Up G Int
-- >>> x1.unNetBytes
-- 7
--
-- >>> let x2 = hideSize x1 :: SomeNetSize Up Int
-- >>> x2.unSomeNetSize
-- 7
--
-- >>> let x3 = (hideDirection x1) :: SomeNetDir G Int
-- >>> x3.unSomeNetDir
-- 7
--
-- >>> let x4 = (hideDirection x2) :: SomeNet Int
-- >>> x4.unSomeNet
-- 7

-- $elimination3
-- Optics are another option. The underscore-prefixed optics unwrap one level
-- at a time, since we can freely compose them.
--
-- >>> import Optics.Core (view, (%))
-- >>> import Data.Bytes (_MkBytes)
-- >>> let x1 = MkNetBytesP 7 :: NetBytes Up G Int
-- >>> view (_MkNetBytes % _MkBytes) x1
-- 7
--
-- >>> -- notice we have to convert the numeric value since the requested
-- >>> -- return type ('M') differs from the original ('G')
-- >>> let x2 = hideSize x1 :: SomeNetSize Up Int
-- >>> (view _MkSomeNetSize x2) :: NetBytes Up M Int
-- MkNetBytes (MkBytes 7000)
--
-- >>> view (_MkSomeNetSize % (_MkNetBytes @M) % _MkBytes) x2
-- 7000
--
-- The @-XOverloadedLabel@ instances unwrap all the way to the underlying numeric
-- value.
--
-- >>> view #unNetBytes x1
-- 7
--
-- >>> view #unSomeNetSize x2
-- 7
--
-- >>> let x3 = hideDirection x1 :: SomeNetDir G Int
-- >>> view #unSomeNetDir x3
-- 7
--
-- >>> let x4 = hideSize x3 :: SomeNet Int
-- >>> view #unSomeNet x4
-- 7

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
-- >>> mb1 .% 10
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
