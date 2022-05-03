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
  ( -- * Types

    -- ** Units
    Size (..),

    -- ** Bytes
    Bytes (..),
    Bytes.bytesToSize,

    -- *** Unknown Size
    SomeSize,
    hideSize,
    Bytes.unSomeSize,
    Bytes.someSizeToSize,

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
import Data.Bytes.Internal (Bytes (..), SomeSize, hideSize)
import Data.Bytes.Internal qualified as Bytes
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
-- "Prettyprinter"'s 'Prettyprinter.Pretty' class can be used for pretty
-- printing.
--
-- >>> -- import Prettyprinter (Pretty (..), layoutPretty, defaultLayoutOptions)
-- >>> -- import Prettyprinter.Render.String (renderString)
-- >>> renderString $ layoutPretty defaultLayoutOptions $ pretty $ MkBytes @G @Float 203.301
-- "203.301 G"

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
-- >>> -- import Numeric.Algebra (ASemigroup ((.+.)), AGroup ((.-.))
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
-- >>> -- import Numeric.Algebra (MSemiSpace ((.*)))
-- >>> mb1 .* 10
-- MkBytes {unBytes = 200}
--
-- === Division
-- >>> -- import Numeric.Algebra (MSpace ((.%)))
-- >>> -- import Numeric.Data.NonZero (unsafeNonZero)
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
