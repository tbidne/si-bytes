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

    -- * Byte Operations
    BytesNum (..),
    LiftBase (..),

    -- * Unknown Size
    AnySize (..),
    mapAny,

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
import ByteTypes.Data.Size
  ( ByteSize (..),
    Conversion (..),
    DecByteSize (..),
    IncByteSize (..),
    NextUnit,
    Normalize (..),
    PrevUnit,
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
  MkB :: n -> Bytes 'B n
  MkKB :: n -> Bytes 'KB n
  MkMB :: n -> Bytes 'MB n
  MkGB :: n -> Bytes 'GB n
  MkTB :: n -> Bytes 'TB n
  MkPB :: n -> Bytes 'PB n

deriving instance Show n => Show (Bytes s n)

deriving instance Functor (Bytes s)

instance Applicative (Bytes 'B) where
  pure = MkB
  MkB f <*> MkB x = MkB $ f x

instance Applicative (Bytes 'KB) where
  pure = MkKB
  MkKB f <*> MkKB x = MkKB $ f x

instance Applicative (Bytes 'MB) where
  pure = MkMB
  MkMB f <*> MkMB x = MkMB $ f x

instance Applicative (Bytes 'GB) where
  pure = MkGB
  MkGB f <*> MkGB x = MkGB $ f x

instance Applicative (Bytes 'TB) where
  pure = MkTB
  MkTB f <*> MkTB x = MkTB $ f x

instance Applicative (Bytes 'PB) where
  pure = MkPB
  MkPB f <*> MkPB x = MkPB $ f x

instance Applicative (Bytes s) => Monad (Bytes s) where
  MkB x >>= f = f x
  MkKB x >>= f = f x
  MkMB x >>= f = f x
  MkGB x >>= f = f x
  MkTB x >>= f = f x
  MkPB x >>= f = f x

instance Fractional n => Conversion (Bytes s n) where
  type BType (Bytes s n) = Bytes 'B n
  type KType (Bytes s n) = Bytes 'KB n
  type MType (Bytes s n) = Bytes 'MB n
  type GType (Bytes s n) = Bytes 'GB n
  type TType (Bytes s n) = Bytes 'TB n
  type PType (Bytes s n) = Bytes 'PB n
  toB (MkB x) = MkB $ Size.convert B B x
  toB (MkKB x) = MkB $ Size.convert KB B x
  toB (MkMB x) = MkB $ Size.convert MB B x
  toB (MkGB x) = MkB $ Size.convert GB B x
  toB (MkTB x) = MkB $ Size.convert TB B x
  toB (MkPB x) = MkB $ Size.convert PB B x
  toKB (MkB x) = MkKB $ Size.convert B KB x
  toKB (MkKB x) = MkKB $ Size.convert KB KB x
  toKB (MkMB x) = MkKB $ Size.convert MB KB x
  toKB (MkGB x) = MkKB $ Size.convert GB KB x
  toKB (MkTB x) = MkKB $ Size.convert TB KB x
  toKB (MkPB x) = MkKB $ Size.convert PB KB x
  toMB (MkB x) = MkMB $ Size.convert B MB x
  toMB (MkKB x) = MkMB $ Size.convert KB MB x
  toMB (MkMB x) = MkMB $ Size.convert MB MB x
  toMB (MkGB x) = MkMB $ Size.convert GB MB x
  toMB (MkTB x) = MkMB $ Size.convert TB MB x
  toMB (MkPB x) = MkMB $ Size.convert PB MB x
  toGB (MkB x) = MkGB $ Size.convert B GB x
  toGB (MkKB x) = MkGB $ Size.convert KB GB x
  toGB (MkMB x) = MkGB $ Size.convert MB GB x
  toGB (MkGB x) = MkGB $ Size.convert GB GB x
  toGB (MkTB x) = MkGB $ Size.convert TB GB x
  toGB (MkPB x) = MkGB $ Size.convert PB GB x
  toTB (MkB x) = MkTB $ Size.convert B TB x
  toTB (MkKB x) = MkTB $ Size.convert KB TB x
  toTB (MkMB x) = MkTB $ Size.convert MB TB x
  toTB (MkGB x) = MkTB $ Size.convert GB TB x
  toTB (MkTB x) = MkTB $ Size.convert TB TB x
  toTB (MkPB x) = MkTB $ Size.convert PB TB x
  toPB (MkB x) = MkPB $ Size.convert B PB x
  toPB (MkKB x) = MkPB $ Size.convert KB PB x
  toPB (MkMB x) = MkPB $ Size.convert MB PB x
  toPB (MkGB x) = MkPB $ Size.convert GB PB x
  toPB (MkTB x) = MkPB $ Size.convert TB PB x
  toPB (MkPB x) = MkPB $ Size.convert PB PB x

instance LiftBase (Bytes s n) where
  type Base (Bytes s n) = n
  liftB f x = getCons x $ f $ unBytes x
  liftB2 f x y = getCons x $ f (unBytes x) (unBytes y)
  liftB3 f x y z = getCons x $ f (unBytes x) (unBytes y) (unBytes z)

instance (Num n) => BytesNum (Bytes s n) where
  type Scalar (Bytes s n) = n
  (|+|) = liftB2 (+)
  (|-|) = liftB2 (-)
  (*|) c = fmap (* c)

instance Fractional n => IncByteSize (Bytes s n) where
  type Next (Bytes s n) = Bytes (NextUnit s) n
  next (MkB x) = MkKB $ x / 1_000
  next (MkKB x) = MkMB $ x / 1_000
  next (MkMB x) = MkGB $ x / 1_000
  next (MkGB x) = MkTB $ x / 1_000
  next (MkTB x) = MkPB $ x / 1_000
  next (MkPB x) = MkPB x

instance Fractional n => DecByteSize (Bytes s n) where
  type Prev (Bytes s n) = Bytes (PrevUnit s) n
  prev (MkB x) = MkB x
  prev (MkKB x) = MkB $ x * 1_000
  prev (MkMB x) = MkKB $ x * 1_000
  prev (MkGB x) = MkMB $ x * 1_000
  prev (MkTB x) = MkGB $ x * 1_000
  prev (MkPB x) = MkTB $ x * 1_000

instance PrintfArg n => PrettyPrint (Bytes a n) where
  pretty (MkB x) = Pf.printf "%.2f" x <> " B"
  pretty (MkKB x) = Pf.printf "%.2f" x <> " KB"
  pretty (MkMB x) = Pf.printf "%.2f" x <> " MB"
  pretty (MkGB x) = Pf.printf "%.2f" x <> " GB"
  pretty (MkTB x) = Pf.printf "%.2f" x <> " TB"
  pretty (MkPB x) = Pf.printf "%.2f" x <> " PB"

instance (Ord n, Fractional n) => Normalize (Bytes s n) where
  type Result (Bytes s n) = AnySize n

  normalize bytes@(MkB x)
    | x < 1 = MkAnySize bytes
  normalize bytes@(MkPB x)
    | x > 999 = MkAnySize bytes
  normalize bytes =
    let n = unBytes bytes
     in if
            | n < 1 -> normalize $ prev bytes
            | n > 999 -> normalize $ next bytes
            | otherwise -> MkAnySize bytes

-- | Unwraps the bytes.
unBytes :: Bytes s n -> n
unBytes (MkB x) = x
unBytes (MkKB x) = x
unBytes (MkMB x) = x
unBytes (MkGB x) = x
unBytes (MkTB x) = x
unBytes (MkPB x) = x

getCons :: Bytes s n -> (n -> Bytes s n)
getCons (MkB _) = MkB
getCons (MkKB _) = MkKB
getCons (MkMB _) = MkMB
getCons (MkGB _) = MkGB
getCons (MkTB _) = MkTB
getCons (MkPB _) = MkPB

-- | Wrapper for 'Bytes', existentially quantifying the size. This is useful
-- when a function does not know a priori what size it should return, e.g.,
--
-- @
--   getFileSize :: IO (AnySize Float)
--   getFileSize path = do
--     (bytes, units) <- getRawFileSize path
--     case units of
--       "B" -> MkAnySize $ MkB bytes
--       "KB" -> MkAnySize $ MkKB bytes
--       ...
-- @
--
-- 'AnySize'\'s 'BytesNum' functions are 'normalize'd.
type AnySize :: Type -> Type
data AnySize n where
  MkAnySize :: Bytes s n -> AnySize n

deriving instance Show n => Show (AnySize n)

deriving instance Functor AnySize

instance Fractional n => Conversion (AnySize n) where
  type BType (AnySize n) = AnySize n
  type KType (AnySize n) = AnySize n
  type MType (AnySize n) = AnySize n
  type GType (AnySize n) = AnySize n
  type TType (AnySize n) = AnySize n
  type PType (AnySize n) = AnySize n
  toB (MkAnySize x) = MkAnySize $ toB x
  toKB (MkAnySize x) = MkAnySize $ toKB x
  toMB (MkAnySize x) = MkAnySize $ toMB x
  toGB (MkAnySize x) = MkAnySize $ toGB x
  toTB (MkAnySize x) = MkAnySize $ toTB x
  toPB (MkAnySize x) = MkAnySize $ toPB x

instance (Fractional n, Ord n) => LiftBase (AnySize n) where
  type Base (AnySize n) = Bytes 'B n
  liftB f (MkAnySize x) = normalize $ f $ toB x
  liftB2 f (MkAnySize x) (MkAnySize y) =
    normalize $ f (toB x) (toB y)
  liftB3 f (MkAnySize x) (MkAnySize y) (MkAnySize z) =
    normalize $ f (toB x) (toB y) (toB z)

instance (Ord n, Fractional n) => BytesNum (AnySize n) where
  type Scalar (AnySize n) = n
  (|+|) = liftB2 (|+|)
  (|-|) = liftB2 (|-|)
  (*|) c = fmap (* c)

instance PrintfArg n => PrettyPrint (AnySize n) where
  pretty (MkAnySize b) = pretty b

instance (Ord n, Fractional n) => Normalize (AnySize n) where
  type Result (AnySize n) = AnySize n
  normalize (MkAnySize x) = normalize x

-- | Maps a 'Bytes' function onto 'AnySize'. The parameter function must be
-- polymorphic in the size. Because this function can change the 'ByteSize',
-- care must be taken. As such, the caller is assumed to know what they are
-- doing, and no normalization is performed.
--
-- Whenever possible, 'liftB' is preferred.
mapAny :: (forall s. Bytes s m -> Bytes t n) -> AnySize m -> AnySize n
mapAny f (MkAnySize x) = MkAnySize $ f x
