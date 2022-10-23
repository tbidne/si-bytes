{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides typeclasses for convert between byte sizes.
--
-- @since 0.1
module Data.Bytes.Class.Conversion
  ( Conversion (..),
    convert',
    convertWitness,
  )
where

import Data.Bytes.Size (SSize (..), SingSize (..), Size (..))
import Data.Proxy (Proxy (..))
import Numeric.Algebra (MGroup (..), MSemigroup (..))
import Numeric.Data.NonZero (NonZero (..), reallyUnsafeNonZero)
import Numeric.Literal.Integer (FromInteger (..))

-- $setup
-- >>> import Data.Bytes.Internal (Bytes (..))
-- >>> import Data.Bytes.Size (Size (..), Sized (..))
-- >>> import Data.Proxy (Proxy (Proxy))

-- | This class allows one to transform a bytes type to any 'Size'. For types
-- with existentially quantified 'Size' (e.g. 'Data.Bytes.SomeSize',
-- 'Data.Bytes.Network.NetBytes.SomeNetSize'), this will "undo" the existential quantification.
--
-- @since 0.1
class Conversion a where
  -- | @since 0.1
  type Converted (t :: Size) a = r | r -> t

  -- | @convert (Proxy :: Proxy t) x@ converts @x@ to size @t@.
  --
  -- ==== __Examples__
  --
  -- >>> let bytes = MkBytes 50_000 :: Bytes 'M Int
  -- >>> let gBytes = convert @_ @G Proxy bytes
  -- >>> :type gBytes
  -- gBytes :: Bytes 'G Int
  -- >>> gBytes
  -- MkBytes 50
  --
  -- >>> let bytes = hideSize (MkBytes 0.2 :: Bytes 'T Float)
  -- >>> let mBytes = convert @_ @M Proxy bytes
  -- >>> :type mBytes
  -- mBytes :: Bytes 'M Float
  -- >>> mBytes
  -- MkBytes 200000.0
  --
  -- @since 0.1
  convert :: SingSize t => Proxy t -> a -> Converted t a

-- | Low level function for converting a numeric literal /from/ the inferred
-- 'SingSize' /to/ the parameter 'Size'. For instance,
--
-- >>> convertWitness @K M 5_000
-- 5
--
-- This is slightly more principled than 'convert', but the higher level
-- byte types and functions should still be preferred
-- (e.g. 'Data.Bytes', 'Data.Bytes.Class.Normalize').
--
-- @since 0.1
convertWitness ::
  forall s n.
  ( FromInteger n,
    MGroup n,
    SingSize s
  ) =>
  Size ->
  n ->
  n
convertWitness toUnits n = case singSize @s of
  SB -> convert' B toUnits n
  SK -> convert' K toUnits n
  SM -> convert' M toUnits n
  SG -> convert' G toUnits n
  ST -> convert' T toUnits n
  SP -> convert' P toUnits n
  SE -> convert' E toUnits n
  SZ -> convert' Z toUnits n
  SY -> convert' Y toUnits n
{-# INLINEABLE convertWitness #-}

-- | Low level function for converting a numeric literal between
-- byte sizes. @convert b1 b2@ converts /from/ @b1@ /to/ @b2@,
-- e.g. @convert G K = \\n -> n * 1_000_000@. The higher level
-- byte types and functions should be preferred
-- (e.g. 'Conversion', "Data.Bytes.Class.Normalize"), but this is
-- here when it is needed.
--
-- @since 0.1
convert' ::
  forall n.
  ( FromInteger n,
    MGroup n
  ) =>
  Size ->
  Size ->
  n ->
  n
convert' B B n = n
convert' B K n = n .%. nzafromInteger @n 1_000
convert' B M n = n .%. nzafromInteger @n 1_000_000
convert' B G n = n .%. nzafromInteger @n 1_000_000_000
convert' B T n = n .%. nzafromInteger @n 1_000_000_000_000
convert' B P n = n .%. nzafromInteger @n 1_000_000_000_000_000
convert' B E n = n .%. nzafromInteger @n 1_000_000_000_000_000_000
convert' B Z n = n .%. nzafromInteger @n 1_000_000_000_000_000_000_000
convert' B Y n = n .%. nzafromInteger @n 1_000_000_000_000_000_000_000_000
convert' K B n = n .*. afromInteger 1_000
convert' K K n = n
convert' K M n = n .%. nzafromInteger @n 1_000
convert' K G n = n .%. nzafromInteger @n 1_000_000
convert' K T n = n .%. nzafromInteger @n 1_000_000_000
convert' K P n = n .%. nzafromInteger @n 1_000_000_000_000
convert' K E n = n .%. nzafromInteger @n 1_000_000_000_000_000
convert' K Z n = n .%. nzafromInteger @n 1_000_000_000_000_000_000
convert' K Y n = n .%. nzafromInteger @n 1_000_000_000_000_000_000_000
convert' M B n = n .*. afromInteger 1_000_000
convert' M K n = n .*. afromInteger 1_000
convert' M M n = n
convert' M G n = n .%. nzafromInteger @n 1_000
convert' M T n = n .%. nzafromInteger @n 1_000_000
convert' M P n = n .%. nzafromInteger @n 1_000_000_000
convert' M E n = n .%. nzafromInteger @n 1_000_000_000_000
convert' M Z n = n .%. nzafromInteger @n 1_000_000_000_000_000
convert' M Y n = n .%. nzafromInteger @n 1_000_000_000_000_000_000
convert' G B n = n .*. afromInteger 1_000_000_000
convert' G K n = n .*. afromInteger 1_000_000
convert' G M n = n .*. afromInteger 1_000
convert' G G n = n
convert' G T n = n .%. nzafromInteger @n 1_000
convert' G P n = n .%. nzafromInteger @n 1_000_000
convert' G E n = n .%. nzafromInteger @n 1_000_000_000
convert' G Z n = n .%. nzafromInteger @n 1_000_000_000_000
convert' G Y n = n .%. nzafromInteger @n 1_000_000_000_000_000
convert' T B n = n .*. afromInteger 1_000_000_000_000
convert' T K n = n .*. afromInteger 1_000_000_000
convert' T M n = n .*. afromInteger 1_000_000
convert' T G n = n .*. afromInteger 1_000
convert' T T n = n
convert' T P n = n .%. nzafromInteger @n 1_000
convert' T E n = n .%. nzafromInteger @n 1_000_000
convert' T Z n = n .%. nzafromInteger @n 1_000_000_000
convert' T Y n = n .%. nzafromInteger @n 1_000_000_000_000
convert' P B n = n .*. afromInteger 1_000_000_000_000_000
convert' P K n = n .*. afromInteger 1_000_000_000_000
convert' P M n = n .*. afromInteger 1_000_000_000
convert' P G n = n .*. afromInteger 1_000_000
convert' P T n = n .*. afromInteger 1_000
convert' P P n = n
convert' P E n = n .%. nzafromInteger @n 1_000
convert' P Z n = n .%. nzafromInteger @n 1_000_000
convert' P Y n = n .%. nzafromInteger @n 1_000_000_000
convert' E B n = n .*. afromInteger 1_000_000_000_000_000_000
convert' E K n = n .*. afromInteger 1_000_000_000_000_000
convert' E M n = n .*. afromInteger 1_000_000_000_000
convert' E G n = n .*. afromInteger 1_000_000_000
convert' E T n = n .*. afromInteger 1_000_000
convert' E P n = n .*. afromInteger 1_000
convert' E E n = n
convert' E Z n = n .%. nzafromInteger @n 1_000
convert' E Y n = n .%. nzafromInteger @n 1_000_000
convert' Z B n = n .*. afromInteger 1_000_000_000_000_000_000_000
convert' Z K n = n .*. afromInteger 1_000_000_000_000_000_000
convert' Z M n = n .*. afromInteger 1_000_000_000_000_000
convert' Z G n = n .*. afromInteger 1_000_000_000_000
convert' Z T n = n .*. afromInteger 1_000_000_000
convert' Z P n = n .*. afromInteger 1_000_000
convert' Z E n = n .*. afromInteger 1_000
convert' Z Z n = n
convert' Z Y n = n .%. nzafromInteger @n 1_000
convert' Y B n = n .*. afromInteger 1_000_000_000_000_000_000_000_000
convert' Y K n = n .*. afromInteger 1_000_000_000_000_000_000_000
convert' Y M n = n .*. afromInteger 1_000_000_000_000_000_000
convert' Y G n = n .*. afromInteger 1_000_000_000_000_000
convert' Y T n = n .*. afromInteger 1_000_000_000_000
convert' Y P n = n .*. afromInteger 1_000_000_000
convert' Y E n = n .*. afromInteger 1_000_000
convert' Y Z n = n .*. afromInteger 1_000
convert' Y Y n = n
{-# INLINEABLE convert' #-}

nzafromInteger :: forall n. FromInteger n => Integer -> NonZero n
nzafromInteger = reallyUnsafeNonZero . afromInteger
{-# INLINE nzafromInteger #-}
