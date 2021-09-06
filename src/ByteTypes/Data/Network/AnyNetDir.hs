-- | This module provides types for hiding the
-- 'ByteTypes.Data.Direction.ByteDirection' on 'NetBytes'.
--
-- This paradigm differs from the previous size hiding
-- 'ByteTypes.Data.Bytes.AnySize' and 'AnyNetSize' in that because we had
-- sensible ways to convert between sizes (e.g. K -> G), we could
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
--   getMaxTraffic :: IO (AnyNetDir K Double)
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
-- MkAnyNetDir SK (MkNetBytes @Up (MkBytes 1000)) == MkAnyNetDir SM (MkNetBytes @Up (MkBytes 1))
-- MkAnyNetDir SK (MkNetBytes @Up (MkBytes 1000)) /= MkAnyNetDir SM (MkNetBytes @Down (MkBytes 1))
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
  type Converted 'K (AnyNetDir s n) = AnyNetDir 'K n
  type Converted 'M (AnyNetDir s n) = AnyNetDir 'M n
  type Converted 'G (AnyNetDir s n) = AnyNetDir 'G n
  type Converted 'T (AnyNetDir s n) = AnyNetDir 'T n
  type Converted 'P (AnyNetDir s n) = AnyNetDir 'P n

  toB (MkAnyNetDir dir x) = MkAnyNetDir dir $ toB x
  toK (MkAnyNetDir dir x) = MkAnyNetDir dir $ toK x
  toM (MkAnyNetDir dir x) = MkAnyNetDir dir $ toM x
  toG (MkAnyNetDir dir x) = MkAnyNetDir dir $ toG x
  toT (MkAnyNetDir dir x) = MkAnyNetDir dir $ toT x
  toP (MkAnyNetDir dir x) = MkAnyNetDir dir $ toP x

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
--       ("Down", "K" -> MkAnyNet SDown SK $ MkBytes bytes
--       ("Up", "M") -> MkAnyNet SUp SM $ MkBytes bytes
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
-- MkAnyNet 'Up 'K (MkNetBytes 1_000) == MkAnyNet 'Up 'M (MkNetBytes 1)
-- MkAnyNet 'Up 'K (MkNetBytes 1_000) /= MkAnyNet 'Down 'M (MkNetBytes 1)
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
  type Converted 'K (AnyNet n) = AnyNetDir 'K n
  type Converted 'M (AnyNet n) = AnyNetDir 'M n
  type Converted 'G (AnyNet n) = AnyNetDir 'G n
  type Converted 'T (AnyNet n) = AnyNetDir 'T n
  type Converted 'P (AnyNet n) = AnyNetDir 'P n

  toB (MkAnyNet dir sz x) = Size.withSingByteSize sz $ toB (MkAnyNetDir dir x)
  toK (MkAnyNet dir sz x) = Size.withSingByteSize sz $ toK (MkAnyNetDir dir x)
  toM (MkAnyNet dir sz x) = Size.withSingByteSize sz $ toM (MkAnyNetDir dir x)
  toG (MkAnyNet dir sz x) = Size.withSingByteSize sz $ toG (MkAnyNetDir dir x)
  toT (MkAnyNet dir sz x) = Size.withSingByteSize sz $ toT (MkAnyNetDir dir x)
  toP (MkAnyNet dir sz x) = Size.withSingByteSize sz $ toP (MkAnyNetDir dir x)

instance (Field n, NumLiteral n, Ord n) => Normalize (AnyNet n) where
  type Norm (AnyNet n) = AnyNet n
  normalize (MkAnyNet dir sz x) =
    case Size.withSingByteSize sz normalize x of
      MkAnyNetSize sz' x' -> MkAnyNet dir sz' x'

instance PrintfArg n => PrettyPrint (AnyNet n) where
  pretty (MkAnyNet dir sz x) =
    Direction.withSingByteDirection dir $
      Size.withSingByteSize sz $ pretty x
