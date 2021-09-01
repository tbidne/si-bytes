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
    Bytes.bytesToSByteSize,

    -- * Network Bytes
    NetBytes (..),
    unNetBytes,
    resizeNetBytes,
    redirNetBytes,
    netToSByteSize,
    netToSByteDirection,

    -- * Unknown Size
    AnyNetSize (..),
    redirAnyNetSizeBytes,
    anyNetSizeToSByteDirection,

    -- * Unknown Size and Direction
    AnyNet (..),

    -- * Modifying Units
    Normalize (..),
    Conversion (..),
    IncByteSize (..),
    DecByteSize (..),
  )
where

import ByteTypes.Class.Math (Isomorphism (..), NumLiteral (..))
import ByteTypes.Class.Math.Algebra
  ( Field (..),
    Group (..),
    Module (..),
    Ring (..),
    VectorSpace (..),
  )
import ByteTypes.Class.Math.Scalar
  ( Scalar,
    ScalarEq (..),
    ScalarNum (..),
    ScalarOrd (..),
  )
import ByteTypes.Data.Bytes (AnySize (..), Bytes (..))
import ByteTypes.Data.Bytes qualified as Bytes
import ByteTypes.Data.Direction
  ( ByteDirection (..),
    SByteDirection (..),
    SingByteDirection (..),
  )
import ByteTypes.Data.Size
  ( ByteSize (..),
    Conversion (..),
    DecByteSize (..),
    IncByteSize (..),
    NextUnit,
    Normalize (..),
    PrevUnit,
    SByteSize (..),
    SingByteSize (..),
  )
import Control.Applicative (liftA2)
import Data.Kind (Type)

-- | Wrapper around the 'Bytes' type that adds the 'ByteDirection' tag.
type NetBytes :: ByteDirection -> ByteSize -> Type -> Type
data NetBytes d s n where
  MkNetBytes :: Bytes s n -> NetBytes d s n

-- | Unwraps the 'NetBytes'.
unNetBytes :: NetBytes d s n -> Bytes s n
unNetBytes (MkNetBytes x) = x

deriving instance Show n => Show (NetBytes d s n)

deriving instance Functor (NetBytes d s)

instance Applicative (NetBytes d s) where
  pure = MkNetBytes . pure
  MkNetBytes f <*> MkNetBytes x = MkNetBytes $ f <*> x

instance Monad (NetBytes d s) where
  MkNetBytes x >>= f = MkNetBytes $ x >>= (unNetBytes . f)

instance Eq n => Eq (NetBytes d s n) where
  MkNetBytes x == MkNetBytes y = x == y

instance Ord n => Ord (NetBytes d s n) where
  MkNetBytes x <= MkNetBytes y = x <= y

type instance Scalar (NetBytes d s n) = n

instance Eq n => ScalarEq (NetBytes d s n) where
  MkNetBytes x .= k = x .= k

instance Ord n => ScalarOrd (NetBytes d s n) where
  MkNetBytes x .<= k = x .<= k

instance Ring n => ScalarNum (NetBytes d s n) where
  MkNetBytes x .+ k = MkNetBytes $ x .+ k

instance Group n => Group (NetBytes d s n) where
  (.+.) = liftA2 (.+.)
  (.-.) = liftA2 (.-.)
  gid = MkNetBytes gid
  ginv = fmap ginv
  gabs = fmap gabs

instance Ring n => Module (NetBytes d s n) n where
  MkNetBytes x .* k = MkNetBytes $ x .* k

instance Field n => VectorSpace (NetBytes d s n) n where
  MkNetBytes x .% k = MkNetBytes $ x .% k

instance Isomorphism (NetBytes d s n) (Bytes s n) where
  to = unNetBytes
  from = MkNetBytes

instance (Field n, NumLiteral n, SingByteSize s) => Conversion (NetBytes d s n) where
  type Converted 'B (NetBytes d s n) = NetBytes d 'B n
  type Converted 'KB (NetBytes d s n) = NetBytes d 'KB n
  type Converted 'MB (NetBytes d s n) = NetBytes d 'MB n
  type Converted 'GB (NetBytes d s n) = NetBytes d 'GB n
  type Converted 'TB (NetBytes d s n) = NetBytes d 'TB n
  type Converted 'PB (NetBytes d s n) = NetBytes d 'PB n

  toB (MkNetBytes b) = MkNetBytes $ toB b
  toKB (MkNetBytes b) = MkNetBytes $ toKB b
  toMB (MkNetBytes b) = MkNetBytes $ toMB b
  toGB (MkNetBytes b) = MkNetBytes $ toGB b
  toTB (MkNetBytes b) = MkNetBytes $ toTB b
  toPB (MkNetBytes b) = MkNetBytes $ toPB b

instance (Field n, NumLiteral n, SingByteSize s) => IncByteSize (NetBytes d s n) where
  type Next (NetBytes d s n) = NetBytes d (NextUnit s) n
  next (MkNetBytes x) = MkNetBytes $ next x

instance (NumLiteral n, Ring n, SingByteSize s) => DecByteSize (NetBytes d s n) where
  type Prev (NetBytes d s n) = NetBytes d (PrevUnit s) n
  prev (MkNetBytes x) = MkNetBytes $ prev x

instance (Field n, NumLiteral n, Ord n, SingByteSize s) => Normalize (NetBytes d s n) where
  type Norm (NetBytes d s n) = AnyNetSize d n

  normalize (MkNetBytes bytes) = case normalize bytes of
    MkAnySize sz bytes' -> MkAnyNetSize sz $ MkNetBytes bytes'

-- | Changes the 'ByteSize' tag.
resizeNetBytes :: NetBytes d s n -> NetBytes d t n
resizeNetBytes = MkNetBytes . Bytes.resizeBytes . unNetBytes

-- | Changes the 'ByteDirection' tag.
redirNetBytes :: NetBytes d s n -> NetBytes e s n
redirNetBytes (MkNetBytes x) = MkNetBytes x

-- | Retrieves the 'SByteDirection' witness.
netToSByteDirection :: SingByteDirection d => NetBytes d s n -> SByteDirection d
netToSByteDirection _ = singByteDirection

-- | Retrieves the 'SingByteSize' witness.
netToSByteSize :: SingByteSize s => NetBytes d s n -> SByteSize s
netToSByteSize _ = singByteSize

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
-- 'AnyNetSize'\'s 'Num' functions are 'normalize'd.
type AnyNetSize :: ByteDirection -> Type -> Type
data AnyNetSize d n where
  MkAnyNetSize :: SByteSize s -> NetBytes d s n -> AnyNetSize d n

deriving instance Show n => Show (AnyNetSize d n)

deriving instance Functor (AnyNetSize d)

instance Eq n => Eq (AnyNetSize d n) where
  MkAnyNetSize szx x == MkAnyNetSize szy y =
    case (szx, szy) of
      (SB, SB) -> x == y
      (SKB, SKB) -> x == y
      (SMB, SMB) -> x == y
      (SGB, SGB) -> x == y
      (STB, STB) -> x == y
      (SPB, SPB) -> x == y
      _ -> False

instance Ord n => Ord (AnyNetSize d n) where
  MkAnyNetSize szx x <= MkAnyNetSize szy y =
    case (szx, szy) of
      (SB, SB) -> x <= y
      (SB, _) -> True
      (SKB, SB) -> False
      (SKB, SKB) -> x <= y
      (SKB, _) -> True
      (SMB, SB) -> False
      (SMB, SKB) -> False
      (SMB, SMB) -> x <= y
      (SMB, _) -> True
      (SGB, SB) -> False
      (SGB, SKB) -> False
      (SGB, SMB) -> False
      (SGB, SGB) -> x <= y
      (SGB, _) -> True
      (STB, SB) -> False
      (STB, SKB) -> False
      (STB, SMB) -> False
      (STB, SGB) -> False
      (STB, STB) -> x <= y
      (STB, _) -> True
      (SPB, SB) -> False
      (SPB, SKB) -> False
      (SPB, SMB) -> False
      (SPB, SGB) -> False
      (SPB, STB) -> False
      (SPB, SPB) -> x <= y

type instance Scalar (AnyNetSize d n) = n

instance Eq n => ScalarEq (AnyNetSize d n) where
  MkAnyNetSize _ x .= k = x .= k

instance Ord n => ScalarOrd (AnyNetSize d n) where
  MkAnyNetSize _ x .<= k = x .<= k

instance (Field n, NumLiteral n, Ord n, Ring n) => ScalarNum (AnyNetSize d n) where
  MkAnyNetSize sz x .+ k = MkAnyNetSize sz $ x .+ k

instance (Field n, NumLiteral n, Ord n) => Group (AnyNetSize d n) where
  x .+. y =
    let x' = to @_ @(NetBytes d 'B n) x
        y' = to y
     in normalize $ x' .+. y'

  x .-. y =
    let x' = to @_ @(NetBytes d 'B n) x
        y' = to y
     in normalize $ x' .-. y'

  gid = MkAnyNetSize SB gid
  ginv = fmap ginv
  gabs = fmap gabs

instance (Field n, NumLiteral n, Ord n, Ring n) => Module (AnyNetSize d n) n where
  MkAnyNetSize sz x .* k = MkAnyNetSize sz $ x .* k

instance (Field n, NumLiteral n, Ord n, Ring n) => VectorSpace (AnyNetSize d n) n where
  MkAnyNetSize sz x .% k = MkAnyNetSize sz $ x .% k

instance (Field n, NumLiteral n, SingByteSize s) => Isomorphism (AnyNetSize d n) (NetBytes d s n) where
  to (MkAnyNetSize sz x) = case (singByteSize @s) of
    SB ->
      case sz of
        SB -> toB x
        SKB -> toB x
        SMB -> toB x
        SGB -> toB x
        STB -> toB x
        SPB -> toB x
    SKB ->
      case sz of
        SB -> toKB x
        SKB -> toKB x
        SMB -> toKB x
        SGB -> toKB x
        STB -> toKB x
        SPB -> toKB x
    SMB ->
      case sz of
        SB -> toMB x
        SKB -> toMB x
        SMB -> toMB x
        SGB -> toMB x
        STB -> toMB x
        SPB -> toMB x
    SGB ->
      case sz of
        SB -> toGB x
        SKB -> toGB x
        SMB -> toGB x
        SGB -> toGB x
        STB -> toGB x
        SPB -> toGB x
    STB ->
      case sz of
        SB -> toTB x
        SKB -> toTB x
        SMB -> toTB x
        SGB -> toTB x
        STB -> toTB x
        SPB -> toTB x
    SPB ->
      case sz of
        SB -> toPB x
        SKB -> toPB x
        SMB -> toPB x
        SGB -> toPB x
        STB -> toPB x
        SPB -> toPB x

  from bytes = MkAnyNetSize (netToSByteSize bytes) bytes

instance (Field n, NumLiteral n) => Conversion (AnyNetSize d n) where
  type Converted _ (AnyNetSize d n) = AnyNetSize d n

  toB (MkAnyNetSize sz x) = case sz of
    SB -> let x' = toB x in MkAnyNetSize SB x'
    SKB -> let x' = toB x in MkAnyNetSize SB x'
    SMB -> let x' = toB x in MkAnyNetSize SB x'
    SGB -> let x' = toB x in MkAnyNetSize SB x'
    STB -> let x' = toB x in MkAnyNetSize SB x'
    SPB -> let x' = toB x in MkAnyNetSize SB x'
  toKB (MkAnyNetSize sz x) = case sz of
    SB -> let x' = toKB x in MkAnyNetSize SKB x'
    SKB -> let x' = toKB x in MkAnyNetSize SKB x'
    SMB -> let x' = toKB x in MkAnyNetSize SKB x'
    SGB -> let x' = toKB x in MkAnyNetSize SKB x'
    STB -> let x' = toKB x in MkAnyNetSize SKB x'
    SPB -> let x' = toKB x in MkAnyNetSize SKB x'
  toMB (MkAnyNetSize sz x) = case sz of
    SB -> let x' = toMB x in MkAnyNetSize SMB x'
    SKB -> let x' = toMB x in MkAnyNetSize SMB x'
    SMB -> let x' = toMB x in MkAnyNetSize SMB x'
    SGB -> let x' = toMB x in MkAnyNetSize SMB x'
    STB -> let x' = toMB x in MkAnyNetSize SMB x'
    SPB -> let x' = toMB x in MkAnyNetSize SMB x'
  toGB (MkAnyNetSize sz x) = case sz of
    SB -> let x' = toGB x in MkAnyNetSize SGB x'
    SKB -> let x' = toGB x in MkAnyNetSize SGB x'
    SMB -> let x' = toGB x in MkAnyNetSize SGB x'
    SGB -> let x' = toGB x in MkAnyNetSize SGB x'
    STB -> let x' = toGB x in MkAnyNetSize SGB x'
    SPB -> let x' = toGB x in MkAnyNetSize SGB x'
  toTB (MkAnyNetSize sz x) = case sz of
    SB -> let x' = toTB x in MkAnyNetSize STB x'
    SKB -> let x' = toTB x in MkAnyNetSize STB x'
    SMB -> let x' = toTB x in MkAnyNetSize STB x'
    SGB -> let x' = toTB x in MkAnyNetSize STB x'
    STB -> let x' = toTB x in MkAnyNetSize STB x'
    SPB -> let x' = toTB x in MkAnyNetSize STB x'
  toPB (MkAnyNetSize sz x) = case sz of
    SB -> let x' = toPB x in MkAnyNetSize SPB x'
    SKB -> let x' = toPB x in MkAnyNetSize SPB x'
    SMB -> let x' = toPB x in MkAnyNetSize SPB x'
    SGB -> let x' = toPB x in MkAnyNetSize SPB x'
    STB -> let x' = toPB x in MkAnyNetSize SPB x'
    SPB -> let x' = toPB x in MkAnyNetSize SPB x'

instance (Field n, NumLiteral n, Ord n) => Normalize (AnyNetSize d n) where
  type Norm (AnyNetSize d n) = AnyNetSize d n
  normalize (MkAnyNetSize sz x) = case sz of
    SB -> normalize x
    SKB -> normalize x
    SMB -> normalize x
    SGB -> normalize x
    STB -> normalize x
    SPB -> normalize x

-- | Changes the 'ByteDirection' tag.
redirAnyNetSizeBytes :: AnyNetSize d n -> AnyNetSize e n
redirAnyNetSizeBytes (MkAnyNetSize sz x) = MkAnyNetSize sz $ redirNetBytes x

-- | Retrieves the 'SingByteDirection' witness.
anyNetSizeToSByteDirection :: SingByteDirection d => AnyNetSize d n -> SByteDirection d
anyNetSizeToSByteDirection _ = singByteDirection

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
-- 'AnyNet'\'s 'Num' functions are 'normalize'd. We deliberately
-- do not provide instances for 'Isomorphism' as combining arbitrary
-- 'AnyNet's would defeat the type's purpose of keeping different
-- directions separate.
type AnyNet :: Type -> Type
data AnyNet n where
  MkAnyNet :: SByteDirection d -> AnyNetSize d n -> AnyNet n

deriving instance Show n => Show (AnyNet n)

deriving instance Functor AnyNet

type instance Scalar (AnyNet n) = n

instance Eq n => ScalarEq (AnyNet n) where
  MkAnyNet _ x .= k = x .= k

instance Ord n => ScalarOrd (AnyNet n) where
  MkAnyNet _ x .<= k = x .<= k

instance (Field n, NumLiteral n) => Conversion (AnyNet n) where
  type Converted _ (AnyNet n) = AnyNet n

  toB (MkAnyNet dir x) = MkAnyNet dir $ toB x
  toKB (MkAnyNet dir x) = MkAnyNet dir $ toKB x
  toMB (MkAnyNet dir x) = MkAnyNet dir $ toMB x
  toGB (MkAnyNet dir x) = MkAnyNet dir $ toGB x
  toTB (MkAnyNet dir x) = MkAnyNet dir $ toTB x
  toPB (MkAnyNet dir x) = MkAnyNet dir $ toPB x

instance (Field n, NumLiteral n, Ord n) => Normalize (AnyNet n) where
  type Norm (AnyNet n) = AnyNet n
  normalize (MkAnyNet dir x) = MkAnyNet dir $ normalize x
