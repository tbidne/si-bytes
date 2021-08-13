{-# LANGUAGE UndecidableInstances #-}

-- | This module provides a slightly more complicated alternative to
-- 'ByteTypes.Data.Bytes', for when there is a need to distinguish between
-- downloaded and uploaded bytes.
module ByteTypes.Data.Network
  ( -- * Tags
    ByteSize (..),
    ByteDirection (..),

    -- * Bytes
    Bytes (..),
    Bytes.unBytes,

    -- * Network Bytes
    NetBytes (..),
    unNetBytes,
    mapNet,

    -- * Byte Operations
    BytesNum (..),
    LiftBase (..),

    -- * Unknown Size
    AnyNetSize (..),
    mapAnyNetSize,

    -- * Unknown Size and Direction
    AnyNet (..),
    mapAnyNet,
    liftAnyNet,

    -- * Modifying Units
    Normalize (..),
    Conversion (..),
    IncByteSize (..),
    DecByteSize (..),

    -- * Printing
    PrettyPrint (..),
  )
where

import ByteTypes.Class.LiftBase (LiftBase (..))
import ByteTypes.Class.Num (BytesNum (..))
import ByteTypes.Class.PrettyPrint (PrettyPrint (..))
import ByteTypes.Data.Bytes (AnySize (..), Bytes (..))
import ByteTypes.Data.Bytes qualified as Bytes
import ByteTypes.Data.Size
  ( ByteSize (..),
    Conversion (..),
    DecByteSize (..),
    IncByteSize (..),
    NextUnit,
    Normalize (..),
    PrevUnit,
  )
import Data.Kind (Type)
import Text.Printf (PrintfArg)

-- | Tags for differentiating downloaded vs. uploaded bytes.
data ByteDirection
  = Down
  | Up
  deriving (Show)

-- | Wrapper around the 'Bytes' type that adds the 'ByteDirection' tag.
data NetBytes :: ByteDirection -> ByteSize -> Type -> Type where
  MkDown :: Bytes s n -> NetBytes 'Down s n
  MkUp :: Bytes s n -> NetBytes 'Up s n

deriving instance Show n => Show (NetBytes d s n)

deriving instance Functor (NetBytes d s)

instance (Applicative (Bytes s)) => Applicative (NetBytes 'Down s) where
  pure = MkDown . pure
  MkDown f <*> MkDown b = MkDown $ f <*> b

instance (Applicative (Bytes s)) => Applicative (NetBytes 'Up s) where
  pure = MkUp . pure
  MkUp f <*> MkUp b = MkUp $ f <*> b

instance
  (Applicative (NetBytes d s), Monad (Bytes s)) =>
  Monad (NetBytes d s)
  where
  MkDown b >>= f = MkDown $ b >>= unNetBytes . f
  MkUp b >>= f = MkUp $ b >>= unNetBytes . f

instance Fractional n => Conversion (NetBytes d s n) where
  type BType (NetBytes d s n) = NetBytes d 'B n
  type KType (NetBytes d s n) = NetBytes d 'KB n
  type MType (NetBytes d s n) = NetBytes d 'MB n
  type GType (NetBytes d s n) = NetBytes d 'GB n
  type TType (NetBytes d s n) = NetBytes d 'TB n
  type PType (NetBytes d s n) = NetBytes d 'PB n
  toB x = getNetAnySizeCons x $ toB $ unNetBytes x
  toKB x = getNetAnySizeCons x $ toKB $ unNetBytes x
  toMB x = getNetAnySizeCons x $ toMB $ unNetBytes x
  toGB x = getNetAnySizeCons x $ toGB $ unNetBytes x
  toTB x = getNetAnySizeCons x $ toTB $ unNetBytes x
  toPB x = getNetAnySizeCons x $ toPB $ unNetBytes x

instance LiftBase (NetBytes d s n) where
  type Base (NetBytes d s n) = Bytes s n
  liftB f x = getNetCons x $ f (unNetBytes x)
  liftB2 f x y =
    getNetCons x $
      f (unNetBytes x) (unNetBytes y)
  liftB3 f x y z =
    getNetCons x $
      f (unNetBytes x) (unNetBytes y) (unNetBytes z)

instance Num n => BytesNum (NetBytes d s n) where
  type Scalar (NetBytes d s n) = n
  (|+|) = liftB2 (|+|)
  (|-|) = liftB2 (|-|)
  (*|) c = fmap (* c)

instance Fractional n => IncByteSize (NetBytes d s n) where
  type Next (NetBytes d s n) = NetBytes d (NextUnit s) n
  next (MkDown x) = MkDown $ next x
  next (MkUp x) = MkUp $ next x

instance Fractional n => DecByteSize (NetBytes d s n) where
  type Prev (NetBytes d s n) = NetBytes d (PrevUnit s) n
  prev (MkDown b) = MkDown $ prev b
  prev (MkUp b) = MkUp $ prev b

instance PrintfArg n => PrettyPrint (NetBytes d s n) where
  pretty (MkDown x) = pretty x <> " Down"
  pretty (MkUp x) = pretty x <> " Up"

instance (Ord n, Fractional n) => Normalize (NetBytes d s n) where
  type Result (NetBytes d s n) = AnyNetSize d n

  normalize (MkDown x) =
    case normalize x of
      MkAnySize x' -> MkAnyNetSize $ MkDown x'
  normalize (MkUp x) =
    case normalize x of
      MkAnySize x' -> MkAnyNetSize $ MkUp x'

-- | Unwraps the 'NetBytes'.
unNetBytes :: NetBytes d s n -> Bytes s n
unNetBytes (MkDown b) = b
unNetBytes (MkUp b) = b

getNetCons :: NetBytes d s n -> (Bytes s n -> NetBytes d s n)
getNetCons (MkDown _) = MkDown
getNetCons (MkUp _) = MkUp

getNetAnySizeCons :: NetBytes d s n -> (forall x. Bytes x n -> NetBytes d x n)
getNetAnySizeCons (MkDown _) = MkDown
getNetAnySizeCons (MkUp _) = MkUp

-- | Maps a 'Bytes' function onto 'NetBytes'. Because this function can change
-- the 'ByteSize', care must be taken. As such, the caller is assumed to know
-- what they are doing.
--
-- Whenever possible, 'liftB' is preferred.
mapNet :: (Bytes s n -> Bytes t n) -> NetBytes d s n -> NetBytes d t n
mapNet f x =
  let cons = getNetAnySizeCons x
      x' = unNetBytes x
   in cons $ f x'

-- | Wrapper for 'NetBytes', existentially quantifying the size. This is useful
-- when a function does not know a priori what size it should return, e.g.,
--
-- @
--   getUpTraffic :: IO (AnyNetSize Up Float)
--   getUpTraffic = do
--     (bytes, units) <- getUpTrafficRaw
--     case units of
--       "B" -> MkAnyNetSize \@Up $ MkB bytes
--       "KB" -> MkAnyNetSize \@Up $ MkKB bytes
--       ...
-- @
--
-- 'AnyNetSize'\'s 'BytesNum' functions are 'normalize'd.
data AnyNetSize :: ByteDirection -> Type -> Type where
  MkAnyNetSize :: NetBytes d s n -> AnyNetSize d n

deriving instance Show n => Show (AnyNetSize d n)

deriving instance Functor (AnyNetSize d)

instance Fractional n => Conversion (AnyNetSize d n) where
  type BType (AnyNetSize d n) = AnyNetSize d n
  type KType (AnyNetSize d n) = AnyNetSize d n
  type MType (AnyNetSize d n) = AnyNetSize d n
  type GType (AnyNetSize d n) = AnyNetSize d n
  type TType (AnyNetSize d n) = AnyNetSize d n
  type PType (AnyNetSize d n) = AnyNetSize d n
  toB (MkAnyNetSize x) = MkAnyNetSize $ toB x
  toKB (MkAnyNetSize x) = MkAnyNetSize $ toKB x
  toMB (MkAnyNetSize x) = MkAnyNetSize $ toMB x
  toGB (MkAnyNetSize x) = MkAnyNetSize $ toGB x
  toTB (MkAnyNetSize x) = MkAnyNetSize $ toTB x
  toPB (MkAnyNetSize x) = MkAnyNetSize $ toPB x

instance (Fractional n, Ord n) => LiftBase (AnyNetSize d n) where
  type Base (AnyNetSize d n) = NetBytes d 'B n
  liftB f (MkAnyNetSize x) = normalize $ f $ toB x
  liftB2 f (MkAnyNetSize x) (MkAnyNetSize y) =
    normalize $ f (toB x) (toB y)
  liftB3 f (MkAnyNetSize x) (MkAnyNetSize y) (MkAnyNetSize z) =
    normalize $ f (toB x) (toB y) (toB z)

instance (Fractional n, Ord n) => BytesNum (AnyNetSize d n) where
  type Scalar (AnyNetSize d n) = n
  (|+|) = liftB2 (|+|)
  (|-|) = liftB2 (|-|)
  (*|) c = fmap (* c)

instance PrintfArg n => PrettyPrint (AnyNetSize d n) where
  pretty (MkAnyNetSize b) = pretty b

instance (Fractional n, Ord n) => Normalize (AnyNetSize d n) where
  type Result (AnyNetSize d n) = AnyNetSize d n
  normalize (MkAnyNetSize x) = normalize x

-- | Maps a 'NetBytes' function onto 'AnyNetSize'. The parameter function must
-- be polymorphic in the size /and/ direction. Because this function can
-- change the 'ByteSize' and 'ByteDirection', care must be taken. As such, the
-- caller is assumed to know what they are doing and no normalization is
-- performed.
--
-- Whenever possible, 'liftB' is preferred.
mapAnyNetSize ::
  (forall e s. NetBytes e s m -> NetBytes f t n) ->
  AnyNetSize d m ->
  AnyNetSize f n
mapAnyNetSize f (MkAnyNetSize x) = MkAnyNetSize $ f x

-- | Wrapper for 'NetBytes', existentially quantifying the size /and/
-- direction. This is useful when a function does not know a priori what
-- direction it should return,
-- e.g.,
--
-- @
--   getMaxTraffic :: IO (AnyNet Double)
--   getMaxTraffic = do
--     (bytes, direction) <- getMaxTrafficRaw
--     case direction of
--       "Down" -> MkAnyNet $ MkDown bytes
--       "Up" -> MkAnyNet $ MkUp bytes
--       ...
-- @
--
-- 'AnyNet'\'s 'BytesNum' functions are 'normalize'd. We deliberately
-- do not provide instances for 'LiftBase' as combining arbitrary
-- 'AnyNet's would defeat the type's purpose of keeping different
-- directions separate.
data AnyNet :: Type -> Type where
  MkAnyNet :: NetBytes d s n -> AnyNet n

deriving instance Show n => Show (AnyNet n)

deriving instance Functor AnyNet

instance Fractional n => Conversion (AnyNet n) where
  type BType (AnyNet n) = AnyNet n
  type KType (AnyNet n) = AnyNet n
  type MType (AnyNet n) = AnyNet n
  type GType (AnyNet n) = AnyNet n
  type TType (AnyNet n) = AnyNet n
  type PType (AnyNet n) = AnyNet n
  toB (MkAnyNet x) = MkAnyNet $ toB x
  toKB (MkAnyNet x) = MkAnyNet $ toKB x
  toMB (MkAnyNet x) = MkAnyNet $ toMB x
  toGB (MkAnyNet x) = MkAnyNet $ toGB x
  toTB (MkAnyNet x) = MkAnyNet $ toTB x
  toPB (MkAnyNet x) = MkAnyNet $ toPB x

instance PrintfArg n => PrettyPrint (AnyNet n) where
  pretty (MkAnyNet b) = pretty b

instance (Fractional n, Ord n) => Normalize (AnyNet n) where
  type Result (AnyNet n) = AnyNet n
  normalize (MkAnyNet x) =
    case normalize x of
      MkAnyNetSize x' -> MkAnyNet x'

-- | Maps a 'NetBytes' function onto 'AnyNetSize'. The parameter function must
-- be polymorphic in the size /and/ direction. Because this function can change
-- the 'ByteSize' and 'ByteDirection', care must be taken. As such, the caller is
-- assumed to know what they are doing and no normalization is performed.
--
-- Whenever possible, 'liftAnyNet' is preferred.
mapAnyNet ::
  (Fractional n, Ord n) =>
  (forall e s. NetBytes e s m -> NetBytes f t n) ->
  AnyNet m ->
  AnyNet n
mapAnyNet f (MkAnyNet x) =
  case normalize (f x) of
    MkAnyNetSize x' -> MkAnyNet x'

-- | Lifts any 'NetBytes' function onto 'AnyNet'. The difference between
-- this and 'mapAnyNet' is that the parameter function need only work for
-- size 'B' (i.e. the types cannot change), and the result is 'normalize'd.
liftAnyNet ::
  (Fractional n, Ord n) =>
  (forall d. NetBytes d 'B n -> NetBytes d 'B n) ->
  AnyNet n ->
  AnyNet n
liftAnyNet f (MkAnyNet x) =
  let x' = toB x
      normalized = normalize $ f x'
   in case normalized of
        MkAnyNetSize y -> MkAnyNet y
