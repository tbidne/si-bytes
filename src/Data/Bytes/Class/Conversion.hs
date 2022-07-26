{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides typeclasses for convert between byte sizes.
--
-- @since 0.1
module Data.Bytes.Class.Conversion
  ( Conversion (..),
    convert,
    convertWitness,
  )
where

import Data.Bytes.Size (SSize (..), SingSize (..), Size (..))
import Numeric.Algebra (MGroup (..), MSemigroup (..))
import Numeric.Class.Literal (NumLiteral (..))
import Numeric.Data.NonZero (NonZero (..), reallyUnsafeNonZero)

-- $setup
-- >>> import Data.Bytes.Internal (Bytes (..), hideSize)

-- | This class allows one to transform a bytes type to any 'Size'. For types
-- with existentially quantified 'Size' (e.g. 'Data.Bytes.SomeSize',
-- 'Data.Bytes.Network.NetBytes.SomeNetSize'), this will "undo" the existential quantification.
--
-- ==== __Examples__
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
--
-- @since 0.1
class Conversion a where
  -- | @since 0.1
  type Converted (b :: Size) a = r | r -> b

  -- | @since 0.1
  toB :: a -> Converted B a

  -- | @since 0.1
  toK :: a -> Converted K a

  -- | @since 0.1
  toM :: a -> Converted M a

  -- | @since 0.1
  toG :: a -> Converted G a

  -- | @since 0.1
  toT :: a -> Converted T a

  -- | @since 0.1
  toP :: a -> Converted P a

  -- | @since 0.1
  toE :: a -> Converted E a

  -- | @since 0.1
  toZ :: a -> Converted Z a

  -- | @since 0.1
  toY :: a -> Converted Y a

-- | Low level function for converting a numeric literal /from/ the inferred
-- 'SingSize' /to/ the parameter 'Size'. For instance,
--
-- >>> convertWitness @K M 5_000
-- 5
--
--
-- This is slightly more principled than 'convert', but the higher level
-- byte types and functions should still be preferred
-- (e.g. 'Data.Bytes', 'Data.Bytes.Class.Normalize').
--
-- @since 0.1
convertWitness ::
  forall s n.
  ( MGroup n,
    NumLiteral n,
    SingSize s
  ) =>
  Size ->
  n ->
  n
convertWitness toUnits n = case singSize @s of
  SB -> convert B toUnits n
  SK -> convert K toUnits n
  SM -> convert M toUnits n
  SG -> convert G toUnits n
  ST -> convert T toUnits n
  SP -> convert P toUnits n
  SE -> convert E toUnits n
  SZ -> convert Z toUnits n
  SY -> convert Y toUnits n
{-# INLINEABLE convertWitness #-}

-- | Low level function for converting a numeric literal between
-- byte sizes. @convert b1 b2@ converts /from/ @b1@ /to/ @b2@,
-- e.g. @convert G K = \\n -> n * 1_000_000@. The higher level
-- byte types and functions should be preferred
-- (e.g. 'Conversion', "Data.Bytes.Class.Normalize"), but this is
-- here when it is needed.
--
-- @since 0.1
convert ::
  forall n.
  ( MGroup n,
    NumLiteral n
  ) =>
  Size ->
  Size ->
  n ->
  n
convert B B n = n
convert B K n = n .%. nzFromLit @n 1_000
convert B M n = n .%. nzFromLit @n 1_000_000
convert B G n = n .%. nzFromLit @n 1_000_000_000
convert B T n = n .%. nzFromLit @n 1_000_000_000_000
convert B P n = n .%. nzFromLit @n 1_000_000_000_000_000
convert B E n = n .%. nzFromLit @n 1_000_000_000_000_000_000
convert B Z n = n .%. nzFromLit @n 1_000_000_000_000_000_000_000
convert B Y n = n .%. nzFromLit @n 1_000_000_000_000_000_000_000_000
convert K B n = n .*. fromLit 1_000
convert K K n = n
convert K M n = n .%. nzFromLit @n 1_000
convert K G n = n .%. nzFromLit @n 1_000_000
convert K T n = n .%. nzFromLit @n 1_000_000_000
convert K P n = n .%. nzFromLit @n 1_000_000_000_000
convert K E n = n .%. nzFromLit @n 1_000_000_000_000_000
convert K Z n = n .%. nzFromLit @n 1_000_000_000_000_000_000
convert K Y n = n .%. nzFromLit @n 1_000_000_000_000_000_000_000
convert M B n = n .*. fromLit 1_000_000
convert M K n = n .*. fromLit 1_000
convert M M n = n
convert M G n = n .%. nzFromLit @n 1_000
convert M T n = n .%. nzFromLit @n 1_000_000
convert M P n = n .%. nzFromLit @n 1_000_000_000
convert M E n = n .%. nzFromLit @n 1_000_000_000_000
convert M Z n = n .%. nzFromLit @n 1_000_000_000_000_000
convert M Y n = n .%. nzFromLit @n 1_000_000_000_000_000_000
convert G B n = n .*. fromLit 1_000_000_000
convert G K n = n .*. fromLit 1_000_000
convert G M n = n .*. fromLit 1_000
convert G G n = n
convert G T n = n .%. nzFromLit @n 1_000
convert G P n = n .%. nzFromLit @n 1_000_000
convert G E n = n .%. nzFromLit @n 1_000_000_000
convert G Z n = n .%. nzFromLit @n 1_000_000_000_000
convert G Y n = n .%. nzFromLit @n 1_000_000_000_000_000
convert T B n = n .*. fromLit 1_000_000_000_000
convert T K n = n .*. fromLit 1_000_000_000
convert T M n = n .*. fromLit 1_000_000
convert T G n = n .*. fromLit 1_000
convert T T n = n
convert T P n = n .%. nzFromLit @n 1_000
convert T E n = n .%. nzFromLit @n 1_000_000
convert T Z n = n .%. nzFromLit @n 1_000_000_000
convert T Y n = n .%. nzFromLit @n 1_000_000_000_000
convert P B n = n .*. fromLit 1_000_000_000_000_000
convert P K n = n .*. fromLit 1_000_000_000_000
convert P M n = n .*. fromLit 1_000_000_000
convert P G n = n .*. fromLit 1_000_000
convert P T n = n .*. fromLit 1_000
convert P P n = n
convert P E n = n .%. nzFromLit @n 1_000
convert P Z n = n .%. nzFromLit @n 1_000_000
convert P Y n = n .%. nzFromLit @n 1_000_000_000
convert E B n = n .*. fromLit 1_000_000_000_000_000_000
convert E K n = n .*. fromLit 1_000_000_000_000_000
convert E M n = n .*. fromLit 1_000_000_000_000
convert E G n = n .*. fromLit 1_000_000_000
convert E T n = n .*. fromLit 1_000_000
convert E P n = n .*. fromLit 1_000
convert E E n = n
convert E Z n = n .%. nzFromLit @n 1_000
convert E Y n = n .%. nzFromLit @n 1_000_000
convert Z B n = n .*. fromLit 1_000_000_000_000_000_000_000
convert Z K n = n .*. fromLit 1_000_000_000_000_000_000
convert Z M n = n .*. fromLit 1_000_000_000_000_000
convert Z G n = n .*. fromLit 1_000_000_000_000
convert Z T n = n .*. fromLit 1_000_000_000
convert Z P n = n .*. fromLit 1_000_000
convert Z E n = n .*. fromLit 1_000
convert Z Z n = n
convert Z Y n = n .%. nzFromLit @n 1_000
convert Y B n = n .*. fromLit 1_000_000_000_000_000_000_000_000
convert Y K n = n .*. fromLit 1_000_000_000_000_000_000_000
convert Y M n = n .*. fromLit 1_000_000_000_000_000_000
convert Y G n = n .*. fromLit 1_000_000_000_000_000
convert Y T n = n .*. fromLit 1_000_000_000_000
convert Y P n = n .*. fromLit 1_000_000_000
convert Y E n = n .*. fromLit 1_000_000
convert Y Z n = n .*. fromLit 1_000
convert Y Y n = n
{-# INLINEABLE convert #-}

nzFromLit :: forall n. NumLiteral n => Integer -> NonZero n
nzFromLit = reallyUnsafeNonZero . fromLit
{-# INLINEABLE nzFromLit #-}
