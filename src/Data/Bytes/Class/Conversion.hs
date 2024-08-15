{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4, 20, 0)
{-# LANGUAGE RequiredTypeArguments #-}
#endif

{- ORMOLU_DISABLE -}

-- | Provides typeclasses for convert between byte sizes.
--
-- @since 0.1
module Data.Bytes.Class.Conversion
  ( -- * High-level
    Conversion (..),

#if MIN_VERSION_base(4, 20, 0)
    convert,
#endif

    -- * Low-level
    convertSize,
    convertWitness,
  )
where

{- ORMOLU_ENABLE -}

import Data.Bytes.Size
  ( SSize (SB, SE, SG, SK, SM, SP, ST, SY, SZ),
    SingSize (singSize),
    Size (B, E, G, K, M, P, T, Y, Z),
  )
import Numeric.Algebra (MGroup ((.%.)), MSemigroup ((.*.)))
import Numeric.Literal.Integer (FromInteger (afromInteger))

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
  -- >>> let gBytes = convert_ @_ @G bytes
  -- >>> :type gBytes
  -- gBytes :: Bytes G Int
  -- >>> gBytes
  -- MkBytes 50
  --
  -- >>> let bytes = hideSize (MkBytes 0.2 :: Bytes 'T Float)
  -- >>> let mBytes = convert_ @_ @M bytes
  -- >>> :type mBytes
  -- mBytes :: Bytes M Float
  -- >>> mBytes
  -- MkBytes 200000.0
  --
  -- @since 0.1
  convert_ :: (SingSize t) => a -> Converted t a

#if MIN_VERSION_base(4, 20, 0)

-- | Alternative to passing around Proxy, using -XRequiredTypeArguments.
--
-- ==== __Examples__
--
-- >>> let bytes = MkBytes 50_000 :: Bytes 'M Int
-- >>> let gBytes = convert G bytes
-- >>> :type gBytes
-- gBytes :: Bytes G Int
-- >>> gBytes
-- MkBytes 50
--
-- @since 0.1
convert ::
  forall t ->
  (SingSize t) =>
  forall a.
  (Conversion a) =>
  a ->
  Converted t a
convert _ = convert_

#endif

-- | Low level function for converting a numeric literal /from/ the inferred
-- 'SingSize' /to/ the parameter 'Size'. For instance,
--
-- >>> convertWitness @K M 5_000
-- 5
--
-- This is slightly more principled than 'convertSize', but the higher level
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
  SB -> convertSize B toUnits n
  SK -> convertSize K toUnits n
  SM -> convertSize M toUnits n
  SG -> convertSize G toUnits n
  ST -> convertSize T toUnits n
  SP -> convertSize P toUnits n
  SE -> convertSize E toUnits n
  SZ -> convertSize Z toUnits n
  SY -> convertSize Y toUnits n
{-# INLINEABLE convertWitness #-}

-- | Low level function for converting a numeric literal between
-- byte sizes. @convert b1 b2@ converts /from/ @b1@ /to/ @b2@,
-- e.g. @convert G K = \\n -> n * 1_000_000@. The higher level
-- byte types and functions should be preferred
-- (e.g. 'Conversion', "Data.Bytes.Class.Normalize"), but this is
-- here when it is needed.
--
-- @since 0.1
convertSize ::
  forall n.
  ( FromInteger n,
    MGroup n
  ) =>
  Size ->
  Size ->
  n ->
  n
convertSize B B n = n
convertSize B K n = n .%. afromInteger 1_000
convertSize B M n = n .%. afromInteger 1_000_000
convertSize B G n = n .%. afromInteger 1_000_000_000
convertSize B T n = n .%. afromInteger 1_000_000_000_000
convertSize B P n = n .%. afromInteger 1_000_000_000_000_000
convertSize B E n = n .%. afromInteger 1_000_000_000_000_000_000
convertSize B Z n = n .%. afromInteger 1_000_000_000_000_000_000_000
convertSize B Y n = n .%. afromInteger 1_000_000_000_000_000_000_000_000
convertSize K B n = n .*. afromInteger 1_000
convertSize K K n = n
convertSize K M n = n .%. afromInteger 1_000
convertSize K G n = n .%. afromInteger 1_000_000
convertSize K T n = n .%. afromInteger 1_000_000_000
convertSize K P n = n .%. afromInteger 1_000_000_000_000
convertSize K E n = n .%. afromInteger 1_000_000_000_000_000
convertSize K Z n = n .%. afromInteger 1_000_000_000_000_000_000
convertSize K Y n = n .%. afromInteger 1_000_000_000_000_000_000_000
convertSize M B n = n .*. afromInteger 1_000_000
convertSize M K n = n .*. afromInteger 1_000
convertSize M M n = n
convertSize M G n = n .%. afromInteger 1_000
convertSize M T n = n .%. afromInteger 1_000_000
convertSize M P n = n .%. afromInteger 1_000_000_000
convertSize M E n = n .%. afromInteger 1_000_000_000_000
convertSize M Z n = n .%. afromInteger 1_000_000_000_000_000
convertSize M Y n = n .%. afromInteger 1_000_000_000_000_000_000
convertSize G B n = n .*. afromInteger 1_000_000_000
convertSize G K n = n .*. afromInteger 1_000_000
convertSize G M n = n .*. afromInteger 1_000
convertSize G G n = n
convertSize G T n = n .%. afromInteger 1_000
convertSize G P n = n .%. afromInteger 1_000_000
convertSize G E n = n .%. afromInteger 1_000_000_000
convertSize G Z n = n .%. afromInteger 1_000_000_000_000
convertSize G Y n = n .%. afromInteger 1_000_000_000_000_000
convertSize T B n = n .*. afromInteger 1_000_000_000_000
convertSize T K n = n .*. afromInteger 1_000_000_000
convertSize T M n = n .*. afromInteger 1_000_000
convertSize T G n = n .*. afromInteger 1_000
convertSize T T n = n
convertSize T P n = n .%. afromInteger 1_000
convertSize T E n = n .%. afromInteger 1_000_000
convertSize T Z n = n .%. afromInteger 1_000_000_000
convertSize T Y n = n .%. afromInteger 1_000_000_000_000
convertSize P B n = n .*. afromInteger 1_000_000_000_000_000
convertSize P K n = n .*. afromInteger 1_000_000_000_000
convertSize P M n = n .*. afromInteger 1_000_000_000
convertSize P G n = n .*. afromInteger 1_000_000
convertSize P T n = n .*. afromInteger 1_000
convertSize P P n = n
convertSize P E n = n .%. afromInteger 1_000
convertSize P Z n = n .%. afromInteger 1_000_000
convertSize P Y n = n .%. afromInteger 1_000_000_000
convertSize E B n = n .*. afromInteger 1_000_000_000_000_000_000
convertSize E K n = n .*. afromInteger 1_000_000_000_000_000
convertSize E M n = n .*. afromInteger 1_000_000_000_000
convertSize E G n = n .*. afromInteger 1_000_000_000
convertSize E T n = n .*. afromInteger 1_000_000
convertSize E P n = n .*. afromInteger 1_000
convertSize E E n = n
convertSize E Z n = n .%. afromInteger 1_000
convertSize E Y n = n .%. afromInteger 1_000_000
convertSize Z B n = n .*. afromInteger 1_000_000_000_000_000_000_000
convertSize Z K n = n .*. afromInteger 1_000_000_000_000_000_000
convertSize Z M n = n .*. afromInteger 1_000_000_000_000_000
convertSize Z G n = n .*. afromInteger 1_000_000_000_000
convertSize Z T n = n .*. afromInteger 1_000_000_000
convertSize Z P n = n .*. afromInteger 1_000_000
convertSize Z E n = n .*. afromInteger 1_000
convertSize Z Z n = n
convertSize Z Y n = n .%. afromInteger 1_000
convertSize Y B n = n .*. afromInteger 1_000_000_000_000_000_000_000_000
convertSize Y K n = n .*. afromInteger 1_000_000_000_000_000_000_000
convertSize Y M n = n .*. afromInteger 1_000_000_000_000_000_000
convertSize Y G n = n .*. afromInteger 1_000_000_000_000_000
convertSize Y T n = n .*. afromInteger 1_000_000_000_000
convertSize Y P n = n .*. afromInteger 1_000_000_000
convertSize Y E n = n .*. afromInteger 1_000_000
convertSize Y Z n = n .*. afromInteger 1_000
convertSize Y Y n = n
{-# INLINEABLE convertSize #-}
