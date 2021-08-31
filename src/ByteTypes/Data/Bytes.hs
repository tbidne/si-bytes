{-# LANGUAGE UndecidableInstances #-}

-- | The main entry point to the library. Provides the types and classes for
-- working with different byte sizes (e.g. B, KB, MB ...). See
-- 'ByteTypes.Data.Network' if there is a need to distinguish between
-- downloaded and uploaded bytes.
module ByteTypes.Data.Bytes
  ( -- * Tags
    ByteSize (..),

    -- * Bytes
    Bytes (..),
    unBytes,
    bytesToSByteSize,

    -- * Unknown Size
    AnySize (..),

    -- * Modifying Units
    Normalize (..),
    Conversion (..),
    IncByteSize (..),
    DecByteSize (..),

    -- * Printing
    PrettyPrint (..),
  )
where

import Control.Applicative (liftA2)
import ByteTypes.Class.Div (Div (..))
import ByteTypes.Class.Isomorphism (Isomorphism (..))
import ByteTypes.Class.PrettyPrint (PrettyPrint (..))
import ByteTypes.Class.ScalarOrd (Scalar, ScalarEq (..), ScalarOrd (..))
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
import ByteTypes.Data.Size qualified as Size
import Data.Kind (Type)
import Text.Printf (PrintfArg (..))
import Text.Printf qualified as Pf

-- | This is the core type for handling type-safe byte operations. It is
-- intended to be used as a simple wrapper over some numerical type,
-- equipped with a unit tag. It is a GADT so we can recover the unit type
-- when necessary.
--
-- To take full advantage of the API (e.g. `normalize`), the underlying
-- numerical type should be 'Fractional' whenever possible.
type Bytes :: ByteSize -> Type -> Type
data Bytes s n where
  MkBytes :: n -> Bytes s n

-- | Unwraps the 'Bytes'.
unBytes :: Bytes s n -> n
unBytes (MkBytes x) = x

deriving instance Show n => Show (Bytes s n)

deriving instance Functor (Bytes s)

instance Applicative (Bytes s) where
  pure = MkBytes
  MkBytes f <*> MkBytes x = MkBytes $ f x

instance Monad (Bytes s) where
  MkBytes x >>= f = f x

instance Eq n => Eq (Bytes s n) where
  MkBytes x == MkBytes y = x == y

instance Ord n => Ord (Bytes s n) where
  MkBytes x <= MkBytes y = x <= y

instance Num n => Num (Bytes s n) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

type instance Scalar (Bytes s n) = n

instance Eq n => ScalarEq (Bytes s n) where
  MkBytes x .= k = x == k

instance Ord n => ScalarOrd (Bytes s n) where
  MkBytes x .<= k = x <= k

instance (Div n, Num n, SingByteSize s) => Conversion (Bytes s n) where
  type Converted 'B (Bytes s n) = Bytes 'B n
  type Converted 'KB (Bytes s n) = Bytes 'KB n
  type Converted 'MB (Bytes s n) = Bytes 'MB n
  type Converted 'GB (Bytes s n) = Bytes 'GB n
  type Converted 'TB (Bytes s n) = Bytes 'TB n
  type Converted 'PB (Bytes s n) = Bytes 'PB n

  toB b@(MkBytes x) = case bytesToSByteSize b of
    SB -> b
    SKB -> MkBytes $ Size.convert KB B x
    SMB -> MkBytes $ Size.convert MB B x
    SGB -> MkBytes $ Size.convert GB B x
    STB -> MkBytes $ Size.convert TB B x
    SPB -> MkBytes $ Size.convert PB B x
  toKB b@(MkBytes x) = case bytesToSByteSize b of
    SB -> MkBytes $ Size.convert B KB x
    SKB -> b
    SMB -> MkBytes $ Size.convert MB KB x
    SGB -> MkBytes $ Size.convert GB KB x
    STB -> MkBytes $ Size.convert TB KB x
    SPB -> MkBytes $ Size.convert PB KB x
  toMB b@(MkBytes x) = case bytesToSByteSize b of
    SB -> MkBytes $ Size.convert B MB x
    SKB -> MkBytes $ Size.convert KB MB x
    SMB -> b
    SGB -> MkBytes $ Size.convert GB MB x
    STB -> MkBytes $ Size.convert TB MB x
    SPB -> MkBytes $ Size.convert PB MB x
  toGB b@(MkBytes x) = case bytesToSByteSize b of
    SB -> MkBytes $ Size.convert B GB x
    SKB -> MkBytes $ Size.convert KB GB x
    SMB -> MkBytes $ Size.convert MB GB x
    SGB -> b
    STB -> MkBytes $ Size.convert TB GB x
    SPB -> MkBytes $ Size.convert PB GB x
  toTB b@(MkBytes x) = case bytesToSByteSize b of
    SB -> MkBytes $ Size.convert B TB x
    SKB -> MkBytes $ Size.convert KB TB x
    SMB -> MkBytes $ Size.convert MB TB x
    SGB -> MkBytes $ Size.convert GB TB x
    STB -> b
    SPB -> MkBytes $ Size.convert PB TB x
  toPB b@(MkBytes x) = case bytesToSByteSize b of
    SB -> MkBytes $ Size.convert B PB x
    SKB -> MkBytes $ Size.convert KB PB x
    SMB -> MkBytes $ Size.convert MB PB x
    SGB -> MkBytes $ Size.convert GB PB x
    STB -> MkBytes $ Size.convert TB PB x
    SPB -> b

instance (Div n, Num n, SingByteSize s) => IncByteSize (Bytes s n) where
  type Next (Bytes s n) = Bytes (NextUnit s) n
  next b@(MkBytes x) = case bytesToSByteSize b of
    SB -> MkBytes $ x % 1_000
    SKB -> MkBytes $ x % 1_000
    SMB -> MkBytes $ x % 1_000
    SGB -> MkBytes $ x % 1_000
    STB -> MkBytes $ x % 1_000
    SPB -> b

instance (Num n, SingByteSize s) => DecByteSize (Bytes s n) where
  type Prev (Bytes s n) = Bytes (PrevUnit s) n
  prev b@(MkBytes x) = case bytesToSByteSize b of
    SB -> b
    SKB -> MkBytes $ x * 1_000
    SMB -> MkBytes $ x * 1_000
    SGB -> MkBytes $ x * 1_000
    STB -> MkBytes $ x * 1_000
    SPB -> MkBytes $ x * 1_000

instance (PrintfArg n, SingByteSize s) => PrettyPrint (Bytes s n) where
  pretty (MkBytes x) = case singByteSize @s of
    SB -> Pf.printf "%.2f" x <> " B"
    SKB -> Pf.printf "%.2f" x <> " KB"
    SMB -> Pf.printf "%.2f" x <> " MB"
    SGB -> Pf.printf "%.2f" x <> " GB"
    STB -> Pf.printf "%.2f" x <> " TB"
    SPB -> Pf.printf "%.2f" x <> " PB"

instance forall n s. (Div n, Num n, Ord n, SingByteSize s) => Normalize (Bytes s n) where
  type Norm (Bytes s n) = AnySize n

  normalize bytes =
    case bytesToSByteSize bytes of
      SB | absBytes .< 1 -> MkAnySize SB bytes
      SPB | absBytes .>= 1000 -> MkAnySize SPB bytes
      SB -> normGeneral
      SKB -> normGeneral
      SMB -> normGeneral
      SGB -> normGeneral
      STB -> normGeneral
      SPB -> normGeneral
    where
      sz = bytesToSByteSize bytes
      absBytes = abs bytes

      normGeneral ::
        ( SingByteSize (NextUnit s),
          SingByteSize (PrevUnit s)
        ) =>
        AnySize n
      normGeneral
        | absBytes .< 1 = normalize $ prev bytes
        | absBytes .>= 1000 = normalize $ next bytes
        | otherwise = MkAnySize sz bytes

-- | Retrieves the 'SByteSize' witness.
bytesToSByteSize :: SingByteSize s => Bytes s n -> SByteSize s
bytesToSByteSize _ = singByteSize

-- | Wrapper for 'Bytes', existentially quantifying the size. This is useful
-- when a function does not know a priori what size it should return, e.g.,
--
-- @
--   getFileSize :: IO (AnySize Float)
--   getFileSize path = do
--     (bytes, units) <- getRawFileSize path
--     case units of
--       "B" -> MkAnySize SB $ MkB bytes
--       "KB" -> MkAnySize SKB $ MkKB bytes
--       ...
-- @
--
-- 'AnySize' carries along an 'SByteSize' runtime witness for when we
-- need to the size. Its 'Num' functions are 'normalize'd.
type AnySize :: Type -> Type
data AnySize n where
  MkAnySize :: SByteSize s -> Bytes s n -> AnySize n

deriving instance Show n => Show (AnySize n)

deriving instance Functor AnySize

instance Eq n => Eq (AnySize n) where
  MkAnySize szx x == MkAnySize szy y =
    case (szx, szy) of
      (SB, SB) -> x == y
      (SKB, SKB) -> x == y
      (SMB, SMB) -> x == y
      (SGB, SGB) -> x == y
      (STB, STB) -> x == y
      (SPB, SPB) -> x == y
      _ -> False

instance Ord n => Ord (AnySize n) where
  MkAnySize szx x <= MkAnySize szy y =
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

instance (Div n, Num n, Ord n) => Num (AnySize n) where
  x + y =
    let x' = to @_ @(Bytes 'B n) x
        y' = to y
     in normalize $ x' + y'
  x - y =
    let x' = to @_ @(Bytes 'B n) x
        y' = to y
     in normalize $ x' - y'
  x * y =
    let x' = to @_ @(Bytes 'B n) x
        y' = to y
     in normalize $ x' * y'
  abs (MkAnySize sz x) = MkAnySize sz $ abs x
  signum (MkAnySize sz x) = MkAnySize sz $ signum x
  fromInteger n = MkAnySize SB $ fromInteger n

type instance Scalar (AnySize n) = n

instance Eq n => ScalarEq (AnySize n) where
  MkAnySize _ x .= k = unBytes x == k

instance Ord n => ScalarOrd (AnySize n) where
  MkAnySize _ x .<= k = unBytes x <= k

instance forall s n. (Div n, Num n, SingByteSize s) => Isomorphism (AnySize n) (Bytes s n) where
  to (MkAnySize sz b) = case (singByteSize @s) of
    SB ->
      case sz of
        SB -> toB b
        SKB -> toB b
        SMB -> toB b
        SGB -> toB b
        STB -> toB b
        SPB -> toB b
    SKB ->
      case sz of
        SB -> toKB b
        SKB -> toKB b
        SMB -> toKB b
        SGB -> toKB b
        STB -> toKB b
        SPB -> toKB b
    SMB ->
      case sz of
        SB -> toMB b
        SKB -> toMB b
        SMB -> toMB b
        SGB -> toMB b
        STB -> toMB b
        SPB -> toMB b
    SGB ->
      case sz of
        SB -> toGB b
        SKB -> toGB b
        SMB -> toGB b
        SGB -> toGB b
        STB -> toGB b
        SPB -> toGB b
    STB ->
      case sz of
        SB -> toTB b
        SKB -> toTB b
        SMB -> toTB b
        SGB -> toTB b
        STB -> toTB b
        SPB -> toTB b
    SPB ->
      case sz of
        SB -> toPB b
        SKB -> toPB b
        SMB -> toPB b
        SGB -> toPB b
        STB -> toPB b
        SPB -> toPB b

  from bytes = MkAnySize (bytesToSByteSize bytes) bytes

instance (Div n, Num n) => Conversion (AnySize n) where
  type Converted _ (AnySize n) = AnySize n

  toB (MkAnySize sz x) = case sz of
    SB -> let x' = toB x in MkAnySize SB x'
    SKB -> let x' = toB x in MkAnySize SB x'
    SMB -> let x' = toB x in MkAnySize SB x'
    SGB -> let x' = toB x in MkAnySize SB x'
    STB -> let x' = toB x in MkAnySize SB x'
    SPB -> let x' = toB x in MkAnySize SB x'
  toKB (MkAnySize sz x) = case sz of
    SB -> let x' = toKB x in MkAnySize SKB x'
    SKB -> let x' = toKB x in MkAnySize SKB x'
    SMB -> let x' = toKB x in MkAnySize SKB x'
    SGB -> let x' = toKB x in MkAnySize SKB x'
    STB -> let x' = toKB x in MkAnySize SKB x'
    SPB -> let x' = toKB x in MkAnySize SKB x'
  toMB (MkAnySize sz x) = case sz of
    SB -> let x' = toMB x in MkAnySize SMB x'
    SKB -> let x' = toMB x in MkAnySize SMB x'
    SMB -> let x' = toMB x in MkAnySize SMB x'
    SGB -> let x' = toMB x in MkAnySize SMB x'
    STB -> let x' = toMB x in MkAnySize SMB x'
    SPB -> let x' = toMB x in MkAnySize SMB x'
  toGB (MkAnySize sz x) = case sz of
    SB -> let x' = toGB x in MkAnySize SGB x'
    SKB -> let x' = toGB x in MkAnySize SGB x'
    SMB -> let x' = toGB x in MkAnySize SGB x'
    SGB -> let x' = toGB x in MkAnySize SGB x'
    STB -> let x' = toGB x in MkAnySize SGB x'
    SPB -> let x' = toGB x in MkAnySize SGB x'
  toTB (MkAnySize sz x) = case sz of
    SB -> let x' = toTB x in MkAnySize STB x'
    SKB -> let x' = toTB x in MkAnySize STB x'
    SMB -> let x' = toTB x in MkAnySize STB x'
    SGB -> let x' = toTB x in MkAnySize STB x'
    STB -> let x' = toTB x in MkAnySize STB x'
    SPB -> let x' = toTB x in MkAnySize STB x'
  toPB (MkAnySize sz x) = case sz of
    SB -> let x' = toPB x in MkAnySize SPB x'
    SKB -> let x' = toPB x in MkAnySize SPB x'
    SMB -> let x' = toPB x in MkAnySize SPB x'
    SGB -> let x' = toPB x in MkAnySize SPB x'
    STB -> let x' = toPB x in MkAnySize SPB x'
    SPB -> let x' = toPB x in MkAnySize SPB x'

instance PrintfArg n => PrettyPrint (AnySize n) where
  pretty (MkAnySize sz b) = case sz of
    SB -> pretty b
    SKB -> pretty b
    SMB -> pretty b
    SGB -> pretty b
    STB -> pretty b
    SPB -> pretty b

instance (Div n, Num n, Ord n) => Normalize (AnySize n) where
  type Norm (AnySize n) = AnySize n
  normalize (MkAnySize sz x) = case sz of
    SB -> normalize x
    SKB -> normalize x
    SMB -> normalize x
    SGB -> normalize x
    STB -> normalize x
    SPB -> normalize x
