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
    liftBytes2,
    liftBytes3,
    monoSize,

    -- * Byte Operations
    BytesNum (..),

    -- * Unknown Size
    AnySize (..),
    mapAny,
    liftAny,
    liftAny2,
    liftAny3,

    -- * Modifying Units
    Normalize (..),
    IncByteSize (..),
    DecByteSize (..),

    -- * Printing
    PrettyPrint (..),
  )
where

import ByteTypes.Class.Normalize (Normalize (..))
import ByteTypes.Class.Num (BytesNum (..))
import ByteTypes.Class.PrettyPrint (PrettyPrint (..))
import ByteTypes.Data.Size
  ( ByteSize (..),
    DecByteSize (..),
    IncByteSize (..),
    NextUnit,
    PrevUnit,
  )
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
data Bytes :: ByteSize -> Type -> Type where
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

instance (Num n) => BytesNum (Bytes s n) where
  type Scalar (Bytes s n) = n
  (|+|) = liftBytes2 (+)
  (|-|) = liftBytes2 (-)
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
  prev (MkKB x) = MkB $ x / 1_000
  prev (MkMB x) = MkKB $ x / 1_000
  prev (MkGB x) = MkMB $ x / 1_000
  prev (MkTB x) = MkGB $ x / 1_000
  prev (MkPB x) = MkTB $ x / 1_000

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

-- | Lifts a binary function onto 'Bytes'. This is a convenient alternative
-- to 'Control.Applicative.liftA2', as the latter unfortunately requires
-- pattern matching on the constructor ('pure' prevents us from being able to
-- implement 'Applicative' for @Bytes s@).
liftBytes2 :: (n -> n -> n) -> Bytes s n -> Bytes s n -> Bytes s n
liftBytes2 f (MkB x) (MkB y) = MkB $ f x y
liftBytes2 f (MkKB x) (MkKB y) = MkKB $ f x y
liftBytes2 f (MkMB x) (MkMB y) = MkMB $ f x y
liftBytes2 f (MkGB x) (MkGB y) = MkGB $ f x y
liftBytes2 f (MkTB x) (MkTB y) = MkTB $ f x y
liftBytes2 f (MkPB x) (MkPB y) = MkPB $ f x y

-- | Lifts a ternary function onto 'Bytes'.
liftBytes3 ::
  (n -> n -> n -> n) ->
  Bytes s n ->
  Bytes s n ->
  Bytes s n ->
  Bytes s n
liftBytes3 f (MkB x) (MkB y) (MkB z) = MkB $ f x y z
liftBytes3 f (MkKB x) (MkKB y) (MkKB z) = MkKB $ f x y z
liftBytes3 f (MkMB x) (MkMB y) (MkMB z) = MkMB $ f x y z
liftBytes3 f (MkGB x) (MkGB y) (MkGB z) = MkGB $ f x y z
liftBytes3 f (MkTB x) (MkTB y) (MkTB z) = MkTB $ f x y z
liftBytes3 f (MkPB x) (MkPB y) (MkPB z) = MkPB $ f x y z

-- | Maps any @Bytes s n@ to @Bytes 'B n@, multiplying the numerical value as
-- necessary. This is useful for when we need to ensure two byte types have
-- the same type, e.g.,
--
-- @
--   -- x :: Bytes s Float, y :: Bytes t Float
--   (monoSize x) |+| (monoSize y)
-- @
monoSize :: Num n => Bytes s n -> Bytes 'B n
monoSize (MkB b) = MkB b
monoSize (MkKB b) = MkB $ b * 1_000
monoSize (MkMB b) = MkB $ b * 1_000_000
monoSize (MkGB b) = MkB $ b * 1_000_000_000
monoSize (MkTB b) = MkB $ b * 1_000_000_000_000
monoSize (MkPB b) = MkB $ b * 1_000_000_000_000_000

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
data AnySize :: Type -> Type where
  MkAnySize :: Bytes s n -> AnySize n

deriving instance Show n => Show (AnySize n)

deriving instance Functor AnySize

instance (Ord n, Fractional n) => BytesNum (AnySize n) where
  type Scalar (AnySize n) = n
  (|+|) = liftAny2 (|+|)
  (|-|) = liftAny2 (|-|)
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
-- Whenever possible, 'liftAny' is preferred.
mapAny :: (forall s. Bytes s m -> Bytes t n) -> AnySize m -> AnySize n
mapAny f (MkAnySize x) = MkAnySize $ f x

-- | Lifts a 'Bytes' function onto 'AnySize'. The difference between this
-- and 'mapAny' is that the parameter function need only work for size 'B'
-- (i.e. the type cannot change), and the result is 'normalize'd.
liftAny ::
  (Fractional n, Ord n) =>
  (Bytes 'B n -> Bytes 'B n) ->
  AnySize n ->
  AnySize n
liftAny f (MkAnySize x) = normalize $ f $ monoSize x

-- | Lifts a binary 'Bytes' function onto 'AnySize' and 'normalize's.
liftAny2 ::
  (Fractional n, Ord n) =>
  (Bytes 'B n -> Bytes 'B n -> Bytes 'B n) ->
  AnySize n ->
  AnySize n ->
  AnySize n
liftAny2 f (MkAnySize x) (MkAnySize y) =
  let x' = monoSize x
      y' = monoSize y
   in normalize $ f x' y'

-- | Lifts a ternary 'Bytes' function onto 'AnySize' and 'normalize's.
liftAny3 ::
  (Fractional n, Ord n) =>
  (Bytes 'B n -> Bytes 'B n -> Bytes 'B n -> Bytes 'B n) ->
  AnySize n ->
  AnySize n ->
  AnySize n ->
  AnySize n
liftAny3 f (MkAnySize x) (MkAnySize y) (MkAnySize z) =
  let x' = monoSize x
      y' = monoSize y
      z' = monoSize z
   in normalize $ f x' y' z'