-- | This module provides types for hiding the
-- 'ByteTypes.Data.Direction.ByteDirection' on 'NetBytes'.
--
-- This paradigm differs from the previous size hiding
-- 'ByteTypes.Data.Bytes.AnySize' and 'AnyNetSize' in that because we had
-- sensible ways to convert between sizes (e.g. KB -> GB), we could
-- combine arbitrary byte types by first converting to a common type.
--
-- Here, there is no sensible way to convert between uploaded and downloaded
-- byte directions by design. These units are meant to be kept separate.
-- While the witnesses allows us to recover the types at will (and we can
-- \"forget\" the direction tag by dropping to 'ByteTypes.Data.Bytes'),
-- we are much more limited in what we can do. For example, we lose instances
-- like 'Applicative', 'ByteTypes.Class.Math.Algebra.Group'.
module ByteTypes.Data.Network.AnyNetDir
  ( AnyNetDir (..),
    AnyNet (..),
  )
where

import ByteTypes.Class.Conversion (Conversion (..))
import ByteTypes.Class.Math.Algebra.Field (Field (..))
import ByteTypes.Class.Math.Algebra.Ring (Ring (..))
import ByteTypes.Class.Math.Literal (NumLiteral (..))
import ByteTypes.Class.Math.Scalar.Num (ScalarNum (..))
import ByteTypes.Class.Math.Scalar.Ord (ScalarEq (..), ScalarOrd (..))
import ByteTypes.Class.Math.Scalar.Scalar (Scalar)
import ByteTypes.Class.Normalize (Normalize (..))
import ByteTypes.Class.PrettyPrint (PrettyPrint (..))
import ByteTypes.Data.Direction (SByteDirection (..))
import ByteTypes.Data.Direction qualified as Direction
import ByteTypes.Data.Network.NetBytes (AnyNetSize (..), NetBytes)
import ByteTypes.Data.Size (ByteSize (..), SByteSize (..), SingByteSize (..))
import ByteTypes.Data.Size qualified as Size
import Data.Kind (Type)
import Text.Printf (PrintfArg (..))

-- | Wrapper for 'NetBytes', existentially quantifying the direction.
-- This is useful when a function does not know a priori what
-- direction it should return,
-- e.g.,
--
-- @
--   getMaxTraffic :: IO (AnyNetDir KB Double)
--   getMaxTraffic = do
--     (bytes, direction) <- getMaxTrafficRaw
--     case direction of
--       "Down" -> MkAnyNetDir SDown $ MkBytes bytes
--       "Up" -> MkAnyNetDir SUp $ MkBytes bytes
--       ...
-- @
--
-- We deliberately do not provide instances for any classes that could be
-- used to combine arbitrary 'AnyNetDir's (e.g. 'Applicative',
-- 'ByteTypes.Class.Math.Algebra.Group'), as that would defeat the purpose of
-- enforcing the distinction between upload and downloaded bytes.
type AnyNetDir :: ByteSize -> Type -> Type
data AnyNetDir s n where
  MkAnyNetDir :: SByteDirection d -> NetBytes d s n -> AnyNetDir s n

deriving instance Show n => Show (AnyNetDir s n)

deriving instance Functor (AnyNetDir s)

-- | Returns true when the two @AnyNetDir@ have the same @ByteDirection@
-- /and/ the underlying @NetBytes@ has the same value.
--
-- @
-- MkAnyNetDir SKB (MkNetBytes @Up (MkBytes 1000)) == MkAnyNetDir SMB (MkNetBytes @Up (MkBytes 1))
-- MkAnyNetDir SKB (MkNetBytes @Up (MkBytes 1000)) /= MkAnyNetDir SMB (MkNetBytes @Down (MkBytes 1))
-- @
--
-- Notice no 'Ord' instance is provided, as we provide no ordering for
-- 'ByteTypes.Data.Direction.ByteDirection'.
instance (Eq n, Field n, NumLiteral n, SingByteSize s) => Eq (AnyNetDir s n) where
  MkAnyNetDir dx x == MkAnyNetDir dy y =
    case (dx, dy) of
      (SDown, SDown) -> x == y
      (SUp, SUp) -> x == y
      _ -> False

type instance Scalar (AnyNetDir s n) = n

instance Eq n => ScalarEq (AnyNetDir s n) where
  MkAnyNetDir _ x .= k = x .= k

instance Ord n => ScalarOrd (AnyNetDir s n) where
  MkAnyNetDir _ x .<= k = x .<= k

instance Ring n => ScalarNum (AnyNetDir s n) where
  MkAnyNetDir sz x .+ k = MkAnyNetDir sz $ x .+ k
  MkAnyNetDir sz x .- k = MkAnyNetDir sz $ x .- k

instance (Field n, NumLiteral n, SingByteSize s) => Conversion (AnyNetDir s n) where
  type Converted 'B (AnyNetDir s n) = AnyNetDir 'B n
  type Converted 'KB (AnyNetDir s n) = AnyNetDir 'KB n
  type Converted 'MB (AnyNetDir s n) = AnyNetDir 'MB n
  type Converted 'GB (AnyNetDir s n) = AnyNetDir 'GB n
  type Converted 'TB (AnyNetDir s n) = AnyNetDir 'TB n
  type Converted 'PB (AnyNetDir s n) = AnyNetDir 'PB n

  toB (MkAnyNetDir dir x) = MkAnyNetDir dir $ toB x
  toKB (MkAnyNetDir dir x) = MkAnyNetDir dir $ toKB x
  toMB (MkAnyNetDir dir x) = MkAnyNetDir dir $ toMB x
  toGB (MkAnyNetDir dir x) = MkAnyNetDir dir $ toGB x
  toTB (MkAnyNetDir dir x) = MkAnyNetDir dir $ toTB x
  toPB (MkAnyNetDir dir x) = MkAnyNetDir dir $ toPB x

instance (Field n, NumLiteral n, Ord n, SingByteSize s) => Normalize (AnyNetDir s n) where
  type Norm (AnyNetDir s n) = AnyNet n
  normalize (MkAnyNetDir dir x) =
    case normalize x of
      MkAnyNetSize sz y -> MkAnyNet dir sz y

instance (PrintfArg n, SingByteSize s) => PrettyPrint (AnyNetDir s n) where
  pretty (MkAnyNetDir dir x) =
    Direction.withSingByteDirection dir $ pretty x

-- | Wrapper for 'NetBytes', existentially quantifying the size /and/
-- direction. This is useful when a function does not know a priori what
-- size or direction it should return,
-- e.g.,
--
-- @
--   getMaxTraffic :: IO (AnyNet Double)
--   getMaxTraffic = do
--     (bytes, direction, size) <- getMaxTrafficRaw
--     case (direction, size) of
--       ("Down", "KB" -> MkAnyNet SDown SKB $ MkBytes bytes
--       ("Up", "MB") -> MkAnyNet SUp SMB $ MkBytes bytes
--       ...
-- @
--
-- 'AnyNet' carries along 'SByteDirection' and 'SByteSize' runtime witnesses
-- for recovering the 'ByteTypes.Data.Direction.ByteDirection'
-- and 'ByteSize', respectively.
--
-- N.B. 'AnyNetSize'\'s instances for lawful typeclasses (e.g. 'Eq', 'Ord')
-- are themselves lawful w.r.t. the notion of equivalence defined
-- in its 'Eq' instance.
type AnyNet :: Type -> Type
data AnyNet n where
  MkAnyNet :: SByteDirection d -> SByteSize s -> NetBytes d s n -> AnyNet n

deriving instance Show n => Show (AnyNet n)

deriving instance Functor AnyNet

-- | Note: This instance uses the same equivalence relation from 'AnyNetSize'
-- w.r.t the size, and also includes an equality check on the direction.
-- Thus we have, for instance,
--
-- @
-- MkAnyNet 'Up 'KB (MkNetBytes 1_000) == MkAnyNet 'Up 'MB (MkNetBytes 1)
-- MkAnyNet 'Up 'KB (MkNetBytes 1_000) /= MkAnyNet 'Down 'MB (MkNetBytes 1)
-- @
instance (Eq n, Field n, NumLiteral n) => Eq (AnyNet n) where
  MkAnyNet dx szx x == MkAnyNet dy szy y =
    Size.withSingByteSize szx $
      Size.withSingByteSize szy $
        case (dx, dy) of
          (SDown, SDown) -> toB x == toB y
          (SUp, SUp) -> toB x == toB y
          _ -> False

instance (Field n, NumLiteral n) => Conversion (AnyNet n) where
  type Converted 'B (AnyNet n) = AnyNetDir 'B n
  type Converted 'KB (AnyNet n) = AnyNetDir 'KB n
  type Converted 'MB (AnyNet n) = AnyNetDir 'MB n
  type Converted 'GB (AnyNet n) = AnyNetDir 'GB n
  type Converted 'TB (AnyNet n) = AnyNetDir 'TB n
  type Converted 'PB (AnyNet n) = AnyNetDir 'PB n

  toB (MkAnyNet dir sz x) = Size.withSingByteSize sz $ toB (MkAnyNetDir dir x)
  toKB (MkAnyNet dir sz x) = Size.withSingByteSize sz $ toKB (MkAnyNetDir dir x)
  toMB (MkAnyNet dir sz x) = Size.withSingByteSize sz $ toMB (MkAnyNetDir dir x)
  toGB (MkAnyNet dir sz x) = Size.withSingByteSize sz $ toGB (MkAnyNetDir dir x)
  toTB (MkAnyNet dir sz x) = Size.withSingByteSize sz $ toTB (MkAnyNetDir dir x)
  toPB (MkAnyNet dir sz x) = Size.withSingByteSize sz $ toPB (MkAnyNetDir dir x)

instance (Field n, NumLiteral n, Ord n) => Normalize (AnyNet n) where
  type Norm (AnyNet n) = AnyNet n
  normalize (MkAnyNet dir sz x) =
    case Size.withSingByteSize sz normalize x of
      MkAnyNetSize sz' x' -> MkAnyNet dir sz' x'

instance PrintfArg n => PrettyPrint (AnyNet n) where
  pretty (MkAnyNet dir sz x) =
    Direction.withSingByteDirection dir $
      Size.withSingByteSize sz $ pretty x
