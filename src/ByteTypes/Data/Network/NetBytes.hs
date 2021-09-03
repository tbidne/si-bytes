-- | Provides the core alternative to 'ByteTypes.Data.Bytes', for when there
-- is a need to distinguish between downloaded and uploaded bytes.
module ByteTypes.Data.Network.NetBytes
  ( -- * Network Bytes
    NetBytes (..),
    unNetBytes,
    netToSByteSize,
    netToSByteDirection,

    -- * Unknown Size
    AnyNetSize (..),
    anyNetSizeToSByteDirection,
  )
where

import ByteTypes.Class.Conversion (Conversion (..), DecByteSize (..), IncByteSize (..))
import ByteTypes.Class.Math.Algebra.Field (Field (..))
import ByteTypes.Class.Math.Algebra.Group (Group (..))
import ByteTypes.Class.Math.Algebra.Module (Module (..))
import ByteTypes.Class.Math.Algebra.Ring (Ring (..))
import ByteTypes.Class.Math.Algebra.VectorSpace (VectorSpace (..))
import ByteTypes.Class.Math.Isomorphism (Isomorphism (..))
import ByteTypes.Class.Math.Literal (NumLiteral (..))
import ByteTypes.Class.Math.Scalar.Num (ScalarNum (..))
import ByteTypes.Class.Math.Scalar.Ord (ScalarEq (..), ScalarOrd (..))
import ByteTypes.Class.Math.Scalar.Scalar (Scalar)
import ByteTypes.Class.Normalize (Normalize (..))
import ByteTypes.Class.PrettyPrint (PrettyPrint (..))
import ByteTypes.Data.Bytes (AnySize (..), Bytes (..))
import ByteTypes.Data.Direction
  ( ByteDirection (..),
    SByteDirection (..),
    SingByteDirection (..),
  )
import ByteTypes.Data.Size
  ( ByteSize (..),
    NextUnit,
    PrevUnit,
    SByteSize (..),
    SingByteSize (..),
  )
import ByteTypes.Data.Size qualified as Size
import Control.Applicative (liftA2)
import Data.Kind (Type)
import Text.Printf (PrintfArg (..))

-- | Wrapper around the 'Bytes' type that adds the 'ByteDirection' tag.
type NetBytes :: ByteDirection -> ByteSize -> Type -> Type
data NetBytes d s n where
  MkNetBytes :: Bytes s n -> NetBytes d s n

-- | Unwraps the 'NetBytes'.
unNetBytes :: NetBytes d s n -> Bytes s n
unNetBytes (MkNetBytes x) = x

-- | Retrieves the 'SByteDirection' witness.
netToSByteDirection :: SingByteDirection d => NetBytes d s n -> SByteDirection d
netToSByteDirection _ = singByteDirection

-- | Retrieves the 'SingByteSize' witness.
netToSByteSize :: SingByteSize s => NetBytes d s n -> SByteSize s
netToSByteSize _ = singByteSize

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
  MkNetBytes x .- k = MkNetBytes $ x .- k

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

instance
  forall d s n.
  (PrintfArg n, SingByteDirection d, SingByteSize s) =>
  PrettyPrint (NetBytes d s n)
  where
  pretty (MkNetBytes x) = case singByteDirection @d of
    SDown -> pretty x <> " Down"
    SUp -> pretty x <> " Up"

-- | Wrapper for 'NetBytes', existentially quantifying the size. This is useful
-- when a function does not know a priori what size it should return, e.g.,
--
-- @
--   getUpTraffic :: IO (AnyNetSize Up Float)
--   getUpTraffic = do
--     (bytes, units) <- getUpTrafficRaw
--     case units of
--       "B" -> MkAnyNetSize SUp $ MkBytes \@B bytes
--       "KB" -> MkAnyNetSize SUp $ MkBytes \@KB bytes
--       ...
-- @
--
-- 'AnyNetSize'\'s 'Num' functions are 'normalize'd.
type AnyNetSize :: ByteDirection -> Type -> Type
data AnyNetSize d n where
  MkAnyNetSize :: SByteSize s -> NetBytes d s n -> AnyNetSize d n

deriving instance Show n => Show (AnyNetSize d n)

deriving instance Functor (AnyNetSize d)

instance (Eq n, Field n, NumLiteral n) => Eq (AnyNetSize d n) where
  x == y = toB x == toB y

instance (Field n, NumLiteral n, Ord n) => Ord (AnyNetSize d n) where
  x <= y = toB x <= toB y

type instance Scalar (AnyNetSize d n) = n

instance Eq n => ScalarEq (AnyNetSize d n) where
  MkAnyNetSize _ x .= k = x .= k

instance Ord n => ScalarOrd (AnyNetSize d n) where
  MkAnyNetSize _ x .<= k = x .<= k

instance Ring n => ScalarNum (AnyNetSize d n) where
  MkAnyNetSize sz x .+ k = MkAnyNetSize sz $ x .+ k
  MkAnyNetSize sz x .- k = MkAnyNetSize sz $ x .- k

instance (Field n, NumLiteral n, Ord n) => Group (AnyNetSize d n) where
  x .+. y = normalize $ toB x .+. toB y
  x .-. y = normalize $ toB x .-. toB y
  gid = MkAnyNetSize SB gid
  ginv = fmap ginv
  gabs = fmap gabs

instance (Field n, NumLiteral n, Ord n) => Module (AnyNetSize d n) n where
  MkAnyNetSize sz x .* k = MkAnyNetSize sz $ x .* k

instance (Field n, NumLiteral n, Ord n) => VectorSpace (AnyNetSize d n) n where
  MkAnyNetSize sz x .% k = MkAnyNetSize sz $ x .% k

instance (Field n, NumLiteral n, SingByteSize s) => Isomorphism (AnyNetSize d n) (NetBytes d s n) where
  to (MkAnyNetSize sz x) = case (singByteSize @s) of
    SB -> Size.withSingByteSize sz $ toB x
    SKB -> Size.withSingByteSize sz $ toKB x
    SMB -> Size.withSingByteSize sz $ toMB x
    SGB -> Size.withSingByteSize sz $ toGB x
    STB -> Size.withSingByteSize sz $ toTB x
    SPB -> Size.withSingByteSize sz $ toPB x

  from bytes = MkAnyNetSize (netToSByteSize bytes) bytes

instance (Field n, NumLiteral n) => Conversion (AnyNetSize d n) where
  type Converted 'B (AnyNetSize d n) = NetBytes d 'B n
  type Converted 'KB (AnyNetSize d n) = NetBytes d 'KB n
  type Converted 'MB (AnyNetSize d n) = NetBytes d 'MB n
  type Converted 'GB (AnyNetSize d n) = NetBytes d 'GB n
  type Converted 'TB (AnyNetSize d n) = NetBytes d 'TB n
  type Converted 'PB (AnyNetSize d n) = NetBytes d 'PB n

  toB (MkAnyNetSize sz x) = Size.withSingByteSize sz $ toB x
  toKB (MkAnyNetSize sz x) = Size.withSingByteSize sz $ toKB x
  toMB (MkAnyNetSize sz x) = Size.withSingByteSize sz $ toMB x
  toGB (MkAnyNetSize sz x) = Size.withSingByteSize sz $ toGB x
  toTB (MkAnyNetSize sz x) = Size.withSingByteSize sz $ toTB x
  toPB (MkAnyNetSize sz x) = Size.withSingByteSize sz $ toPB x

instance (Field n, NumLiteral n, Ord n) => Normalize (AnyNetSize d n) where
  type Norm (AnyNetSize d n) = AnyNetSize d n
  normalize (MkAnyNetSize sz x) = Size.withSingByteSize sz $ normalize x

instance (PrintfArg n, SingByteDirection d) => PrettyPrint (AnyNetSize d n) where
  pretty (MkAnyNetSize sz b) = case sz of
    SB -> pretty b
    SKB -> pretty b
    SMB -> pretty b
    SGB -> pretty b
    STB -> pretty b
    SPB -> pretty b

-- | Retrieves the 'SingByteDirection' witness.
anyNetSizeToSByteDirection :: SingByteDirection d => AnyNetSize d n -> SByteDirection d
anyNetSizeToSByteDirection _ = singByteDirection
