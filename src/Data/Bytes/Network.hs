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
  ( -- * Types

    -- ** Units
    Size (..),
    Direction (..),

    -- ** Bytes
    NetBytes (..),
    NetBytes.unNetBytesP,
    NetBytes.netToSize,
    NetBytes.netToDirection,

    -- *** Unknown Size
    SomeNetSize,
    hideNetSize,
    NetBytes.unSomeNetSize,
    NetBytes.someNetSizeToSize,
    NetBytes.someNetSizeToDirection,

    -- *** Unknown Direction
    SomeNetDir,
    SomeNetDir.hideNetDir,
    SomeNetDir.unSomeNetDir,
    SomeNetDir.someNetDirToSize,
    SomeNetDir.someNetDirToDirection,

    -- *** Unknown Size and Direction
    SomeNet,
    SomeNetDir.hideNetSizeDir,
    SomeNetDir.unSomeNet,
    SomeNetDir.someNetToSize,
    SomeNetDir.someNetToDirection,

    -- * Transformations

    -- ** Pretty Printing
    -- $pretty

    -- ** Converting Units
    Conversion (..),

    -- ** Normalization
    Normalize (..),

    -- * Algebra
    -- $algebra
  )
where

import Data.Bytes.Class.Conversion (Conversion (..))
import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Network.Direction (Direction (..))
import Data.Bytes.Network.NetBytes (NetBytes (..), SomeNetSize, hideNetSize)
import Data.Bytes.Network.NetBytes qualified as NetBytes
import Data.Bytes.Network.SomeNetDir (SomeNet, SomeNetDir)
import Data.Bytes.Network.SomeNetDir qualified as SomeNetDir
import Data.Bytes.Size (Size (..))

-- $setup
-- >>> import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
-- >>> import Numeric.Algebra.Additive.AGroup (AGroup ((.-.)))
-- >>> import Numeric.Algebra.Space.Semimodule (Semimodule)
-- >>> import Numeric.Algebra.Space.MSemiSpace (MSemiSpace ((.*)))
-- >>> import Numeric.Algebra.Space.MSpace (MSpace ((.%)))
-- >>> import Numeric.Data.NonZero (unsafeNonZero)
-- >>> import Prettyprinter (Pretty (..), layoutPretty, defaultLayoutOptions)
-- >>> import Prettyprinter.Render.String (renderString)

-- $pretty
--
-- "Prettyprinter"'s 'Prettyprinter.Pretty' class can be used for pretty
-- printing.
--
-- >>> -- import Prettyprinter (Pretty (..), layoutPretty, defaultLayoutOptions)
-- >>> -- import Prettyprinter.Render.String (renderString)
-- >>> renderString $ layoutPretty defaultLayoutOptions $ pretty $ MkNetBytesP @Up @G @Float 203.301
-- "203.301 G Up"

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
-- >>> -- import Numeric.Algebra (ASemigroup ((.+.)), AGroup ((.-.))
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
-- >>> -- import Numeric.Algebra (MSemiSpace ((.*)))
-- >>> mb1 .* 10
-- MkNetBytesP {unNetBytesP = 200}
--
-- === Division
-- >>> -- import Numeric.Algebra (MSpace ((.%)))
-- >>> -- import Numeric.Data.NonZero (unsafeNonZero)
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
--
-- This respects 'SomeNetSize'\'s equivalence-class base 'Eq'.
