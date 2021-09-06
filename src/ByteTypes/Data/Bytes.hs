-- | The main entry point to the library. Provides the types and classes for
-- working with different byte sizes (e.g. B, K, M ...). See
-- 'ByteTypes.Data.Network' if there is a need to distinguish between
-- downloaded and uploaded bytes.
module ByteTypes.Data.Bytes
  ( -- * Bytes
    Bytes (..),
    unBytes,
    bytesToSSize,

    -- * Unknown Size
    AnySize (..),
  )
where

import ByteTypes.Class.Conversion
  ( Conversion (..),
    DecSize (..),
    IncSize (..),
  )
import ByteTypes.Class.Conversion qualified as Conv
import ByteTypes.Class.Math.Algebra.Field (Field (..))
import ByteTypes.Class.Math.Algebra.Group (Group (..))
import ByteTypes.Class.Math.Algebra.Module (Module (..))
import ByteTypes.Class.Math.Algebra.Ring (Ring (..))
import ByteTypes.Class.Math.Algebra.VectorSpace (VectorSpace (..))
import ByteTypes.Class.Math.Literal (NumLiteral (..))
import ByteTypes.Class.Math.Scalar.Num (ScalarNum (..))
import ByteTypes.Class.Math.Scalar.Ord (ScalarEq (..), ScalarOrd (..))
import ByteTypes.Class.Math.Scalar.Scalar (Scalar)
import ByteTypes.Class.Normalize (Normalize (..))
import ByteTypes.Class.PrettyPrint (PrettyPrint (..))
import ByteTypes.Data.Size
  ( NextSize,
    PrevSize,
    SSize (..),
    SingSize (..),
    Size (..),
  )
import ByteTypes.Data.Size qualified as Size
import Control.Applicative (liftA2)
import Data.Kind (Type)
import Text.Printf (PrintfArg (..))
import Text.Printf qualified as Pf

-- | This is the core type for handling type-safe byte operations. It is
-- intended to be used as a simple wrapper over some numerical type,
-- equipped with a unit tag. If the units are unknown they can be recovered
-- at runtime via 'bytesToSSize'.
--
-- To take full advantage of the API (e.g. `normalize`), the underlying
-- numerical type should implement 'Field'.
type Bytes :: Size -> Type -> Type
data Bytes s n where
  MkBytes :: n -> Bytes s n

-- | Unwraps the 'Bytes'.
unBytes :: Bytes s n -> n
unBytes (MkBytes x) = x

-- | Changes the 'Size' tag.
resizeBytes :: Bytes s n -> Bytes t n
resizeBytes (MkBytes x) = MkBytes x

-- | Retrieves the 'SSize' witness.
bytesToSSize :: SingSize s => Bytes s n -> SSize s
bytesToSSize _ = singSize

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

type instance Scalar (Bytes s n) = n

instance Eq n => ScalarEq (Bytes s n) where
  MkBytes x .= k = x == k

instance Ord n => ScalarOrd (Bytes s n) where
  MkBytes x .<= k = x <= k

instance Ring n => ScalarNum (Bytes s n) where
  MkBytes x .+ k = MkBytes $ x .+. k
  MkBytes x .- k = MkBytes $ x .-. k

instance Group n => Group (Bytes s n) where
  (.+.) = liftA2 (.+.)
  (.-.) = liftA2 (.-.)
  gid = MkBytes gid
  ginv = fmap ginv
  gabs = fmap gabs

instance Ring n => Module (Bytes s n) n where
  MkBytes x .* k = MkBytes $ x .*. k

instance Field n => VectorSpace (Bytes s n) n where
  MkBytes x .% k = MkBytes $ x .%. k

instance (Field n, NumLiteral n, SingSize s) => Conversion (Bytes s n) where
  type Converted 'B (Bytes s n) = Bytes 'B n
  type Converted 'K (Bytes s n) = Bytes 'K n
  type Converted 'M (Bytes s n) = Bytes 'M n
  type Converted 'G (Bytes s n) = Bytes 'G n
  type Converted 'T (Bytes s n) = Bytes 'T n
  type Converted 'P (Bytes s n) = Bytes 'P n

  toB (MkBytes x) = MkBytes $ Conv.convertWitness @s B x
  toK (MkBytes x) = MkBytes $ Conv.convertWitness @s K x
  toM (MkBytes x) = MkBytes $ Conv.convertWitness @s M x
  toG (MkBytes x) = MkBytes $ Conv.convertWitness @s G x
  toT (MkBytes x) = MkBytes $ Conv.convertWitness @s T x
  toP (MkBytes x) = MkBytes $ Conv.convertWitness @s P x

type instance NextSize (Bytes 'B n) = Bytes 'K n

type instance NextSize (Bytes 'K n) = Bytes 'M n

type instance NextSize (Bytes 'M n) = Bytes 'G n

type instance NextSize (Bytes 'G n) = Bytes 'T n

type instance NextSize (Bytes 'T n) = Bytes 'P n

instance (Field n, NumLiteral n) => IncSize (Bytes 'B n) where
  next x = resizeBytes $ x .% fromLit @n 1_000

instance (Field n, NumLiteral n) => IncSize (Bytes 'K n) where
  next x = resizeBytes $ x .% fromLit @n 1_000

instance (Field n, NumLiteral n) => IncSize (Bytes 'M n) where
  next x = resizeBytes $ x .% fromLit @n 1_000

instance (Field n, NumLiteral n) => IncSize (Bytes 'G n) where
  next x = resizeBytes $ x .% fromLit @n 1_000

instance (Field n, NumLiteral n) => IncSize (Bytes 'T n) where
  next x = resizeBytes $ x .% fromLit @n 1_000

type instance PrevSize (Bytes 'K n) = Bytes 'B n

type instance PrevSize (Bytes 'M n) = Bytes 'K n

type instance PrevSize (Bytes 'G n) = Bytes 'M n

type instance PrevSize (Bytes 'T n) = Bytes 'G n

type instance PrevSize (Bytes 'P n) = Bytes 'T n

instance (NumLiteral n, Ring n) => DecSize (Bytes 'K n) where
  prev x = resizeBytes $ x .* fromLit @n 1_000

instance (NumLiteral n, Ring n) => DecSize (Bytes 'M n) where
  prev x = resizeBytes $ x .* fromLit @n 1_000

instance (NumLiteral n, Ring n) => DecSize (Bytes 'G n) where
  prev x = resizeBytes $ x .* fromLit @n 1_000

instance (NumLiteral n, Ring n) => DecSize (Bytes 'T n) where
  prev x = resizeBytes $ x .* fromLit @n 1_000

instance (NumLiteral n, Ring n) => DecSize (Bytes 'P n) where
  prev x = resizeBytes $ x .* fromLit @n 1_000

instance (Field n, NumLiteral n, Ord n, SingSize s) => Normalize (Bytes s n) where
  type Norm (Bytes s n) = AnySize n

  normalize bytes =
    case bytesToSSize bytes of
      SB
        | absBytes .< fromLit 1_000 -> MkAnySize SB bytes
        | otherwise -> normalize $ next bytes
      SP
        | absBytes .>= fromLit 1 -> MkAnySize SP bytes
        | otherwise -> normalize $ prev bytes
      SK
        | absBytes .< fromLit 1 -> normalize $ prev bytes
        | absBytes .>= fromLit 1_000 -> normalize $ next bytes
        | otherwise -> MkAnySize sz bytes
      SM
        | absBytes .< fromLit 1 -> normalize $ prev bytes
        | absBytes .>= fromLit 1_000 -> normalize $ next bytes
        | otherwise -> MkAnySize sz bytes
      SG
        | absBytes .< fromLit 1 -> normalize $ prev bytes
        | absBytes .>= fromLit 1_000 -> normalize $ next bytes
        | otherwise -> MkAnySize sz bytes
      ST
        | absBytes .< fromLit 1 -> normalize $ prev bytes
        | absBytes .>= fromLit 1_000 -> normalize $ next bytes
        | otherwise -> MkAnySize sz bytes
    where
      sz = bytesToSSize bytes
      absBytes = gabs bytes

instance (PrintfArg n, SingSize s) => PrettyPrint (Bytes s n) where
  pretty (MkBytes x) = case singSize @s of
    SB -> Pf.printf "%.2f" x <> " B"
    SK -> Pf.printf "%.2f" x <> " K"
    SM -> Pf.printf "%.2f" x <> " M"
    SG -> Pf.printf "%.2f" x <> " G"
    ST -> Pf.printf "%.2f" x <> " T"
    SP -> Pf.printf "%.2f" x <> " P"

-- | Wrapper for 'Bytes', existentially quantifying the size. This is useful
-- when a function does not know a priori what size it should return, e.g.,
--
-- @
--   getFileSize :: IO (AnySize Float)
--   getFileSize path = do
--     (bytes, units) <- getRawFileSize path
--     case units of
--       "B" -> MkAnySize SB $ MkB bytes
--       "K" -> MkAnySize SK $ MkK bytes
--       ...
-- @
--
-- 'AnySize' carries along an 'SSize' runtime witness for when we
-- need the size. Its 'Group' functions are 'normalize'd.
--
-- N.B. 'AnySize'\'s instances for lawful typeclasses (e.g. 'Eq', 'Ord',
-- 'Group') are themselves lawful w.r.t. the notion of equivalence defined
-- in its 'Eq' instance.
type AnySize :: Type -> Type
data AnySize n where
  MkAnySize :: SSize s -> Bytes s n -> AnySize n

deriving instance Show n => Show (AnySize n)

deriving instance Functor AnySize

-- | Note: This instance defines an equivalence relation on 'AnySize' that
-- takes units into account. For instance,
--
-- @
-- MkAnySize SK (MkBytes 1000) == MkAnySize SM (MkBytes 1).
-- @
--
-- Because we expose the underlying @Bytes@ in several ways (e.g. 'Show',
-- the 'SSize' witness), this is technically unlawful for equality
-- as it breaks the substitutivity law:
--
-- \[
-- x = y \implies f(x) = f(y).
-- \]
--
-- For instance:
--
-- @
-- let x = MkAnySize SK (MkBytes 1000)
-- let y = MkAnySize SM (MkBytes 1)
-- x == y
-- isK x /= isK y
-- @
--
-- With apologies to Leibniz, such comparisons are too useful to ignore
-- and enable us to implement other lawful classes (e.g. 'Group') that respect
-- this notion of equivalence.
instance (Eq n, Field n, NumLiteral n) => Eq (AnySize n) where
  x == y = toB x == toB y

-- | Like the 'Eq' instance, this instance compares both the numeric value
-- __and__ label, so that, e.g.,
--
-- @
-- MkAnySize SK (MkBytes 5_000) <= MkAnySize SM (MkBytes 8)
-- MkAnySize SM (MkBytes 2) <= MkAnySize SK (MkBytes 5_000)
-- @
instance (Field n, NumLiteral n, Ord n) => Ord (AnySize n) where
  x <= y = toB x <= toB y

instance (Field n, NumLiteral n, Ord n) => Group (AnySize n) where
  x .+. y = normalize $ toB x .+. toB y
  x .-. y = normalize $ toB x .-. toB y
  gid = MkAnySize SB gid
  ginv = fmap ginv
  gabs = fmap gabs

instance (Field n, NumLiteral n, Ord n) => Module (AnySize n) n where
  MkAnySize sz x .* k = MkAnySize sz $ x .* k

instance (Field n, NumLiteral n, Ord n) => VectorSpace (AnySize n) n where
  MkAnySize sz x .% k = MkAnySize sz $ x .% k

instance (Field n, NumLiteral n) => Conversion (AnySize n) where
  type Converted 'B (AnySize n) = Bytes 'B n
  type Converted 'K (AnySize n) = Bytes 'K n
  type Converted 'M (AnySize n) = Bytes 'M n
  type Converted 'G (AnySize n) = Bytes 'G n
  type Converted 'T (AnySize n) = Bytes 'T n
  type Converted 'P (AnySize n) = Bytes 'P n

  toB (MkAnySize sz x) = Size.withSingSize sz $ toB x
  toK (MkAnySize sz x) = Size.withSingSize sz $ toK x
  toM (MkAnySize sz x) = Size.withSingSize sz $ toM x
  toG (MkAnySize sz x) = Size.withSingSize sz $ toG x
  toT (MkAnySize sz x) = Size.withSingSize sz $ toT x
  toP (MkAnySize sz x) = Size.withSingSize sz $ toP x

instance (Field n, NumLiteral n, Ord n) => Normalize (AnySize n) where
  type Norm (AnySize n) = AnySize n
  normalize (MkAnySize sz x) = Size.withSingSize sz $ normalize x

instance PrintfArg n => PrettyPrint (AnySize n) where
  pretty (MkAnySize sz b) = Size.withSingSize sz $ pretty b
