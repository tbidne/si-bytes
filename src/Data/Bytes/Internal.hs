{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal module for "Data.Bytes". The primary difference is
-- this module exposes some underlying details i.e. singleton witnesses.
-- These are hidden by default as they complicate the API.
--
-- @since 0.1
module Data.Bytes.Internal
  ( -- * Bytes
    Bytes (..),
    bytesToSize,
    bytesToSSize,
    bytesLens,

    -- * Unknown Size
    SomeSize (..),
    unSomeSize,
    hideSize,
    someSizeToSize,
    someSizeLens,
  )
where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Data.Bytes.Class.Conversion (Conversion (..))
import Data.Bytes.Class.Conversion qualified as Conv
import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Size
  ( NextSize,
    PrevSize,
    SSize (..),
    SingSize (..),
    Size (..),
  )
import Data.Bytes.Size qualified as Size
import Data.Kind (Type)
#if !MIN_VERSION_prettyprinter(1, 7, 1)
import Data.Text.Prettyprint.Doc (Pretty (..), (<+>))
#endif
import GHC.Generics (Generic)
import Numeric.Algebra
  ( AGroup (..),
    AMonoid (..),
    ASemigroup (..),
    Field,
    MGroup (..),
    MSemiSpace (..),
    MSemigroup (..),
    MSpace (..),
    Module,
    Normed (..),
    Ring,
    Semifield,
    Semimodule,
    Semiring,
    SemivectorSpace,
    VectorSpace,
  )
import Numeric.Class.Literal (NumLiteral (..))
import Numeric.Data.NonZero (reallyUnsafeNonZero)
import Optics.Core (Lens, lens)
#if MIN_VERSION_prettyprinter(1, 7, 1)
import Prettyprinter (Pretty (..), (<+>))
#endif

-- $setup
-- >>> getRawFileSize _ = pure (40, "K")

-- | This is the core type for handling type-safe byte operations. It is
-- intended to be used as a simple wrapper over some numeric type,
-- equipped with a 'Size' tag.
--
-- To take full advantage of the API (e.g. `normalize`), the underlying
-- numeric type should implement 'Semifield' or, ideally, 'Field'.
--
-- ==== __Examples__
-- >>> MkBytes @M 1000
-- MkBytes {unBytes = 1000}
--
-- @since 0.1
type Bytes :: Size -> Type -> Type
newtype Bytes (s :: Size) (n :: Type) = MkBytes
  { -- | Unwraps the 'Bytes'.
    --
    -- @since 0.1
    unBytes :: n
  }
  deriving stock
    ( -- | @since 0.1
      Generic
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | Changes the 'Size' tag.
--
-- @since 0.1
resizeBytes :: Bytes s n -> Bytes t n
resizeBytes (MkBytes x) = MkBytes x
{-# INLINEABLE resizeBytes #-}

-- | Recovers the size.
--
-- ==== __Examples__
--
-- >>> bytesToSize $ MkBytes @G 10
-- G
--
-- @since 0.1
bytesToSize :: SingSize s => Bytes s n -> Size
bytesToSize = Size.ssizeToSize . bytesToSSize
{-# INLINEABLE bytesToSize #-}

-- | Retrieves the 'SSize' witness. Can be used to recover the 'Size'.
--
-- >>> bytesToSSize (MkBytes @K @Int 7)
-- SK
--
-- @since 0.1
bytesToSSize :: SingSize s => Bytes s n -> SSize s
bytesToSSize _ = singSize
{-# INLINEABLE bytesToSSize #-}

-- | @since 0.1
bytesLens :: Lens (Bytes s m) (Bytes s n) m n
bytesLens = lens unBytes (\_ x -> MkBytes x)
{-# INLINEABLE bytesLens #-}

-- | @since 0.1
deriving stock instance Show n => Show (Bytes s n)

-- | @since 0.1
deriving stock instance Functor (Bytes s)

-- | @since 0.1
instance Applicative (Bytes s) where
  pure = MkBytes
  {-# INLINEABLE pure #-}
  MkBytes f <*> MkBytes x = MkBytes $ f x
  {-# INLINEABLE (<*>) #-}

-- | @since 0.1
instance Monad (Bytes s) where
  MkBytes x >>= f = f x
  {-# INLINEABLE (>>=) #-}

-- | @since 0.1
instance Eq n => Eq (Bytes s n) where
  MkBytes x == MkBytes y = x == y
  {-# INLINEABLE (==) #-}

-- | @since 0.1
instance Ord n => Ord (Bytes s n) where
  MkBytes x <= MkBytes y = x <= y
  {-# INLINEABLE (<=) #-}

-- | @since 0.1
instance ASemigroup n => ASemigroup (Bytes s n) where
  (.+.) = liftA2 (.+.)
  {-# INLINEABLE (.+.) #-}

-- | @since 0.1
instance AMonoid n => AMonoid (Bytes s n) where
  zero = MkBytes zero
  {-# INLINEABLE zero #-}

-- | @since 0.1
instance AGroup n => AGroup (Bytes s n) where
  (.-.) = liftA2 (.-.)
  {-# INLINEABLE (.-.) #-}

-- | @since 0.1
instance MSemigroup n => MSemiSpace (Bytes s n) n where
  MkBytes x .* k = MkBytes $ x .*. k
  {-# INLINEABLE (.*) #-}

-- | @since 0.1
instance MGroup n => MSpace (Bytes s n) n where
  MkBytes x .% k = MkBytes $ x .%. k
  {-# INLINEABLE (.%) #-}

-- | @since 0.1
instance Normed n => Normed (Bytes s n) where
  norm (MkBytes x) = MkBytes (norm x)
  {-# INLINEABLE norm #-}

-- | @since 0.1
instance Semiring n => Semimodule (Bytes s n) n

-- | @since 0.1
instance Ring n => Module (Bytes s n) n

-- | @since 0.1
instance Semifield n => SemivectorSpace (Bytes s n) n

-- | @since 0.1
instance Field n => VectorSpace (Bytes s n) n

-- | @since 0.1
instance
  ( MGroup n,
    NumLiteral n,
    SingSize s
  ) =>
  Conversion (Bytes s n)
  where
  type Converted 'B (Bytes s n) = Bytes 'B n
  type Converted 'K (Bytes s n) = Bytes 'K n
  type Converted 'M (Bytes s n) = Bytes 'M n
  type Converted 'G (Bytes s n) = Bytes 'G n
  type Converted 'T (Bytes s n) = Bytes 'T n
  type Converted 'P (Bytes s n) = Bytes 'P n
  type Converted 'E (Bytes s n) = Bytes 'E n
  type Converted 'Z (Bytes s n) = Bytes 'Z n
  type Converted 'Y (Bytes s n) = Bytes 'Y n

  toB (MkBytes x) = MkBytes $ Conv.convertWitness @s B x
  {-# INLINEABLE toB #-}
  toK (MkBytes x) = MkBytes $ Conv.convertWitness @s K x
  {-# INLINEABLE toK #-}
  toM (MkBytes x) = MkBytes $ Conv.convertWitness @s M x
  {-# INLINEABLE toM #-}
  toG (MkBytes x) = MkBytes $ Conv.convertWitness @s G x
  {-# INLINEABLE toG #-}
  toT (MkBytes x) = MkBytes $ Conv.convertWitness @s T x
  {-# INLINEABLE toT #-}
  toP (MkBytes x) = MkBytes $ Conv.convertWitness @s P x
  {-# INLINEABLE toP #-}
  toE (MkBytes x) = MkBytes $ Conv.convertWitness @s E x
  {-# INLINEABLE toE #-}
  toZ (MkBytes x) = MkBytes $ Conv.convertWitness @s Z x
  {-# INLINEABLE toZ #-}
  toY (MkBytes x) = MkBytes $ Conv.convertWitness @s Y x
  {-# INLINEABLE toY #-}

-- | @since 0.1
instance forall n s. (MGroup n, Normed n, NumLiteral n, Ord n, SingSize s) => Normalize (Bytes s n) where
  type Norm (Bytes s n) = SomeSize n

  normalize bytes@(MkBytes x) =
    case bytesToSSize bytes of
      SB
        | absBytes < fromLit 1_000 -> MkSomeSize SB bytes
        | otherwise -> normalize $ incSize bytes
      SY
        | absBytes >= fromLit 1 -> MkSomeSize SY bytes
        | otherwise -> normalize $ decSize bytes
      SK
        | absBytes < fromLit 1 -> normalize $ decSize bytes
        | absBytes >= fromLit 1_000 -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
      SM
        | absBytes < fromLit 1 -> normalize $ decSize bytes
        | absBytes >= fromLit 1_000 -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
      SG
        | absBytes < fromLit 1 -> normalize $ decSize bytes
        | absBytes >= fromLit 1_000 -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
      ST
        | absBytes < fromLit 1 -> normalize $ decSize bytes
        | absBytes >= fromLit 1_000 -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
      SP
        | absBytes < fromLit 1 -> normalize $ decSize bytes
        | absBytes >= fromLit 1_000 -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
      SE
        | absBytes < fromLit 1 -> normalize $ decSize bytes
        | absBytes >= fromLit 1_000 -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
      SZ
        | absBytes < fromLit 1 -> normalize $ decSize bytes
        | absBytes >= fromLit 1_000 -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
    where
      sz = bytesToSSize bytes
      absBytes = norm x
  {-# INLINEABLE normalize #-}

-- | @since 0.1
instance (Pretty n, SingSize s) => Pretty (Bytes s n) where
  pretty (MkBytes x) = case singSize @s of
    SB -> pretty x <+> pretty @String "B"
    SK -> pretty x <+> pretty @String "K"
    SM -> pretty x <+> pretty @String "M"
    SG -> pretty x <+> pretty @String "G"
    ST -> pretty x <+> pretty @String "T"
    SP -> pretty x <+> pretty @String "P"
    SE -> pretty x <+> pretty @String "E"
    SZ -> pretty x <+> pretty @String "Z"
    SY -> pretty x <+> pretty @String "Y"
  {-# INLINEABLE pretty #-}

-- | Wrapper for 'Bytes', existentially quantifying the size. This is useful
-- when a function does not know a priori what size it should return e.g.
--
-- >>> :{
--   getFileSize :: FilePath -> IO (SomeSize Float)
--   getFileSize path = do
--     -- getRawFileSize :: FilePath -> IO (Float, String)
--     (bytes, units) <- getRawFileSize path
--     pure $ case units of
--       "B" -> hideSize $ MkBytes @B bytes
--       "K" -> hideSize $ MkBytes @K bytes
--       _ -> error "todo"
-- :}
--
-- 'SomeSize' carries along an 'SSize' runtime witness for when we
-- need the size. Its 'Numeric.Algebra' functions are 'normalize'd.
--
-- We define an equivalence relation on 'SomeSize' that takes units into
-- account. For instance,
--
-- >>> hideSize (MkBytes @G 7) == hideSize (MkBytes @M 7_000)
-- True
--
-- Because we expose the underlying @Bytes@ in several ways (e.g. 'Show',
-- the 'SSize' witness), this is technically unlawful for equality
-- as it breaks the extensionality law:
--
-- \[
-- x = y \implies f(x) = f(y).
-- \]
--
-- @since 0.1
type SomeSize :: Type -> Type
data SomeSize (n :: Type) where
  -- | @since 0.1
  MkSomeSize :: SSize s -> Bytes s n -> SomeSize n

-- | Unwraps the 'SomeSize'.
--
-- @since 0.1
unSomeSize :: SomeSize n -> n
unSomeSize (MkSomeSize _ b) = unBytes b
{-# INLINEABLE unSomeSize #-}

-- | Wraps a 'Bytes' in an existentially quantified 'SomeSize'.
--
-- @since 0.1
hideSize :: forall s n. SingSize s => Bytes s n -> SomeSize n
hideSize bytes = case singSize @s of
  SB -> MkSomeSize SB bytes
  SK -> MkSomeSize SK bytes
  SM -> MkSomeSize SM bytes
  SG -> MkSomeSize SG bytes
  ST -> MkSomeSize ST bytes
  SP -> MkSomeSize SP bytes
  SE -> MkSomeSize SE bytes
  SZ -> MkSomeSize SZ bytes
  SY -> MkSomeSize SY bytes
{-# INLINEABLE hideSize #-}

-- | Recovers the size.
--
-- ==== __Examples__
--
-- >>> someSizeToSize $ hideSize $ MkBytes @G 10
-- G
--
-- @since 0.1
someSizeToSize :: SomeSize n -> Size
someSizeToSize (MkSomeSize sz _) = Size.ssizeToSize sz
{-# INLINEABLE someSizeToSize #-}

-- | @since 0.1
deriving stock instance Show n => Show (SomeSize n)

-- | @since 0.1
deriving stock instance Functor SomeSize

-- | @since 0.1
instance (Eq n, MGroup n, NumLiteral n) => Eq (SomeSize n) where
  x == y = toB x == toB y
  {-# INLINEABLE (==) #-}

-- | @since 0.1
instance (MGroup n, NumLiteral n, Ord n) => Ord (SomeSize n) where
  x <= y = toB x <= toB y
  {-# INLINEABLE (<=) #-}

-- | @since 0.1
someSizeLens :: Lens (SomeSize m) (SomeSize n) m n
someSizeLens = lens unSomeSize (\(MkSomeSize sz _) x -> MkSomeSize sz (MkBytes x))
{-# INLINEABLE someSizeLens #-}

-- | @since 0.1
instance (ASemigroup n, MGroup n, Normed n, NumLiteral n, Ord n) => ASemigroup (SomeSize n) where
  x .+. y = normalize $ toB x .+. toB y
  {-# INLINEABLE (.+.) #-}

-- | @since 0.1
instance (Normed n, NumLiteral n, Ord n, Semifield n) => AMonoid (SomeSize n) where
  zero = MkSomeSize SB zero
  {-# INLINEABLE zero #-}

-- | @since 0.1
instance (Field n, Normed n, NumLiteral n, Ord n) => AGroup (SomeSize n) where
  x .-. y = normalize $ toB x .-. toB y
  {-# INLINEABLE (.-.) #-}

-- | @since 0.1
instance (MGroup n, Normed n, NumLiteral n, Ord n) => MSemiSpace (SomeSize n) n where
  MkSomeSize sz x .* k = normalize $ MkSomeSize sz $ x .* k
  {-# INLINEABLE (.*) #-}

-- | @since 0.1
instance (MGroup n, Normed n, NumLiteral n, Ord n) => MSpace (SomeSize n) n where
  MkSomeSize sz x .% k = normalize $ MkSomeSize sz $ x .% k
  {-# INLINEABLE (.%) #-}

-- | @since 0.1
instance Normed n => Normed (SomeSize n) where
  norm (MkSomeSize sz x) = MkSomeSize sz (norm x)
  {-# INLINEABLE norm #-}

-- | @since 0.1
instance (Normed n, NumLiteral n, Ord n, Semifield n) => Semimodule (SomeSize n) n

-- | @since 0.1
instance (Field n, Normed n, NumLiteral n, Ord n) => Module (SomeSize n) n

-- | @since 0.1
instance (Normed n, NumLiteral n, Ord n, Semifield n) => SemivectorSpace (SomeSize n) n

-- | @since 0.1
instance (Field n, Normed n, NumLiteral n, Ord n) => VectorSpace (SomeSize n) n

-- | @since 0.1
instance (MGroup n, NumLiteral n) => Conversion (SomeSize n) where
  type Converted 'B (SomeSize n) = Bytes 'B n
  type Converted 'K (SomeSize n) = Bytes 'K n
  type Converted 'M (SomeSize n) = Bytes 'M n
  type Converted 'G (SomeSize n) = Bytes 'G n
  type Converted 'T (SomeSize n) = Bytes 'T n
  type Converted 'P (SomeSize n) = Bytes 'P n
  type Converted 'E (SomeSize n) = Bytes 'E n
  type Converted 'Z (SomeSize n) = Bytes 'Z n
  type Converted 'Y (SomeSize n) = Bytes 'Y n

  toB (MkSomeSize sz x) = Size.withSingSize sz $ toB x
  {-# INLINEABLE toB #-}
  toK (MkSomeSize sz x) = Size.withSingSize sz $ toK x
  {-# INLINEABLE toK #-}
  toM (MkSomeSize sz x) = Size.withSingSize sz $ toM x
  {-# INLINEABLE toM #-}
  toG (MkSomeSize sz x) = Size.withSingSize sz $ toG x
  {-# INLINEABLE toG #-}
  toT (MkSomeSize sz x) = Size.withSingSize sz $ toT x
  {-# INLINEABLE toT #-}
  toP (MkSomeSize sz x) = Size.withSingSize sz $ toP x
  {-# INLINEABLE toP #-}
  toE (MkSomeSize sz x) = Size.withSingSize sz $ toE x
  {-# INLINEABLE toE #-}
  toZ (MkSomeSize sz x) = Size.withSingSize sz $ toZ x
  {-# INLINEABLE toZ #-}
  toY (MkSomeSize sz x) = Size.withSingSize sz $ toY x
  {-# INLINEABLE toY #-}

-- | @since 0.1
instance (MGroup n, Normed n, NumLiteral n, Ord n) => Normalize (SomeSize n) where
  type Norm (SomeSize n) = SomeSize n
  normalize (MkSomeSize sz x) = Size.withSingSize sz $ normalize x
  {-# INLINEABLE normalize #-}

-- | @since 0.1
instance Pretty n => Pretty (SomeSize n) where
  pretty (MkSomeSize sz b) = Size.withSingSize sz $ pretty b
  {-# INLINEABLE pretty #-}

-- | Increases 'Bytes' to the next size.
--
-- ==== __Examples__
--
-- >>> incSize $ MkBytes @M @Float 2_500
-- MkBytes {unBytes = 2.5}
--
-- >>> -- type error: "The byte unit Y does not have a 'next size'."
-- >>> --incSize $ MkBytes @Y @Float 2_500
--
-- @since 0.1
incSize :: forall s n. (MGroup n, NumLiteral n) => Bytes s n -> Bytes (NextSize s) n
incSize = resizeBytes . MkBytes . (.%. nz1000) . unBytes
  where
    nz1000 = reallyUnsafeNonZero $ fromLit 1_000
{-# INLINEABLE incSize #-}

-- | Decreases 'Bytes' to the previous size.
--
-- ==== __Examples__
--
-- >>> decSize $ MkBytes @M @Float 2.5
-- MkBytes {unBytes = 2500.0}
--
-- >>> -- type error: "The byte unit B does not have a 'previous size'."
-- >>> --decSize $ MkBytes @B @Float 2.5
--
-- @since 0.1
decSize :: forall s n. (MSemigroup n, NumLiteral n) => Bytes s n -> Bytes (PrevSize s) n
decSize = resizeBytes . MkBytes . (.*. fromLit @n 1_000) . unBytes
{-# INLINEABLE decSize #-}
