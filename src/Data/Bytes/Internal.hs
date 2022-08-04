{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal module for "Data.Bytes". The primary difference is
-- this module exposes some underlying details i.e. singleton witnesses.
-- These are hidden by default as they complicate the API.
--
-- @since 0.1
module Data.Bytes.Internal
  ( -- * Bytes
    Bytes (..),
    bytesToSSize,

    -- * Unknown Size
    SomeSize (..),
    hideSize,
  )
where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Data.Bytes.Class.Conversion (Conversion (..))
import Data.Bytes.Class.Conversion qualified as Conv
import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Class.Parser (Parser (..))
import Data.Bytes.Class.Parser qualified as Parser
import Data.Bytes.Size
  ( NextSize,
    PrevSize,
    SSize (..),
    SingSize (..),
    Size (..),
    Sized (..),
  )
import Data.Bytes.Size qualified as Size
import Data.Kind (Type)
import Data.Text (Text)
#if !MIN_VERSION_prettyprinter(1, 7, 1)
import Data.Text.Prettyprint.Doc (Pretty (..), (<+>))
#endif
import Data.Bytes.Class.Wrapper (Unwrapper (..))
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
import Numeric.Data.NonZero (reallyUnsafeNonZero)
import Numeric.Literal.Integer (FromInteger (..))
import Numeric.Literal.Rational (FromRational (..))
import Optics.Core (A_Lens, An_Iso, LabelOptic (..), iso, lens)
#if MIN_VERSION_prettyprinter(1, 7, 1)
import Prettyprinter (Pretty (..), (<+>))
#endif
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

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
-- MkBytes 1000
--
-- @since 0.1
type Bytes :: Size -> Type -> Type
newtype Bytes (s :: Size) (n :: Type) = MkBytes n
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Functor,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
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
{-# INLINE resizeBytes #-}

-- | Retrieves the 'SSize' witness. Can be used to recover the 'Size'.
--
-- >>> bytesToSSize (MkBytes @K @Int 7)
-- SK
--
-- @since 0.1
bytesToSSize :: SingSize s => Bytes s n -> SSize s
bytesToSSize _ = singSize
{-# INLINE bytesToSSize #-}

-- | @since 0.1
instance (k ~ An_Iso, a ~ m, b ~ n) => LabelOptic "unBytes" k (Bytes s m) (Bytes s n) a b where
  labelOptic = iso unwrap MkBytes
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance Applicative (Bytes s) where
  pure = MkBytes
  {-# INLINE pure #-}
  MkBytes f <*> MkBytes x = MkBytes $ f x
  {-# INLINE (<*>) #-}

-- | @since 0.1
instance Monad (Bytes s) where
  MkBytes x >>= f = f x
  {-# INLINE (>>=) #-}

-- | @since 0.1
instance FromInteger n => FromInteger (Bytes s n) where
  afromInteger = MkBytes . afromInteger
  {-# INLINE afromInteger #-}

-- | @since 0.1
instance FromRational n => FromRational (Bytes s n) where
  afromRational = MkBytes . afromRational
  {-# INLINE afromRational #-}

-- | @since 0.1
instance ASemigroup n => ASemigroup (Bytes s n) where
  (.+.) = liftA2 (.+.)
  {-# INLINE (.+.) #-}

-- | @since 0.1
instance AMonoid n => AMonoid (Bytes s n) where
  zero = MkBytes zero
  {-# INLINE zero #-}

-- | @since 0.1
instance AGroup n => AGroup (Bytes s n) where
  (.-.) = liftA2 (.-.)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance MSemigroup n => MSemiSpace (Bytes s n) n where
  MkBytes x .* k = MkBytes $ x .*. k
  {-# INLINE (.*) #-}

-- | @since 0.1
instance MGroup n => MSpace (Bytes s n) n where
  MkBytes x .% k = MkBytes $ x .%. k
  {-# INLINEABLE (.%) #-}

-- | @since 0.1
instance Normed n => Normed (Bytes s n) where
  norm (MkBytes x) = MkBytes (norm x)
  {-# INLINE norm #-}

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
  ( FromInteger n,
    MGroup n,
    SingSize s
  ) =>
  Conversion (Bytes s n)
  where
  type Converted B (Bytes s n) = Bytes B n
  type Converted K (Bytes s n) = Bytes K n
  type Converted M (Bytes s n) = Bytes M n
  type Converted G (Bytes s n) = Bytes G n
  type Converted T (Bytes s n) = Bytes T n
  type Converted P (Bytes s n) = Bytes P n
  type Converted E (Bytes s n) = Bytes E n
  type Converted Z (Bytes s n) = Bytes Z n
  type Converted Y (Bytes s n) = Bytes Y n

  toB (MkBytes x) = MkBytes $ Conv.convertWitness @s B x
  {-# INLINE toB #-}
  toK (MkBytes x) = MkBytes $ Conv.convertWitness @s K x
  {-# INLINE toK #-}
  toM (MkBytes x) = MkBytes $ Conv.convertWitness @s M x
  {-# INLINE toM #-}
  toG (MkBytes x) = MkBytes $ Conv.convertWitness @s G x
  {-# INLINE toG #-}
  toT (MkBytes x) = MkBytes $ Conv.convertWitness @s T x
  {-# INLINE toT #-}
  toP (MkBytes x) = MkBytes $ Conv.convertWitness @s P x
  {-# INLINE toP #-}
  toE (MkBytes x) = MkBytes $ Conv.convertWitness @s E x
  {-# INLINE toE #-}
  toZ (MkBytes x) = MkBytes $ Conv.convertWitness @s Z x
  {-# INLINE toZ #-}
  toY (MkBytes x) = MkBytes $ Conv.convertWitness @s Y x
  {-# INLINE toY #-}

-- | @since 0.1
instance
  forall n s.
  (FromInteger n, MGroup n, Normed n, Ord n, SingSize s) =>
  Normalize (Bytes s n)
  where
  type Norm (Bytes s n) = SomeSize n

  normalize bytes@(MkBytes x) =
    case bytesToSSize bytes of
      SB
        | absBytes < afromInteger 1_000 -> MkSomeSize SB bytes
        | otherwise -> normalize $ incSize bytes
      SY
        | absBytes >= afromInteger 1 -> MkSomeSize SY bytes
        | otherwise -> normalize $ decSize bytes
      SK
        | absBytes < afromInteger 1 -> normalize $ decSize bytes
        | absBytes >= afromInteger 1_000 -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
      SM
        | absBytes < afromInteger 1 -> normalize $ decSize bytes
        | absBytes >= afromInteger 1_000 -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
      SG
        | absBytes < afromInteger 1 -> normalize $ decSize bytes
        | absBytes >= afromInteger 1_000 -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
      ST
        | absBytes < afromInteger 1 -> normalize $ decSize bytes
        | absBytes >= afromInteger 1_000 -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
      SP
        | absBytes < afromInteger 1 -> normalize $ decSize bytes
        | absBytes >= afromInteger 1_000 -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
      SE
        | absBytes < afromInteger 1 -> normalize $ decSize bytes
        | absBytes >= afromInteger 1_000 -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
      SZ
        | absBytes < afromInteger 1 -> normalize $ decSize bytes
        | absBytes >= afromInteger 1_000 -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
    where
      sz = bytesToSSize bytes
      absBytes = norm x
  {-# INLINEABLE normalize #-}

-- | @since 0.1
instance (Pretty n, SingSize s) => Pretty (Bytes s n) where
  pretty (MkBytes x) = case singSize @s of
    SB -> pretty x <+> pretty @Text "B"
    SK -> pretty x <+> pretty @Text "K"
    SM -> pretty x <+> pretty @Text "M"
    SG -> pretty x <+> pretty @Text "G"
    ST -> pretty x <+> pretty @Text "T"
    SP -> pretty x <+> pretty @Text "P"
    SE -> pretty x <+> pretty @Text "E"
    SZ -> pretty x <+> pretty @Text "Z"
    SY -> pretty x <+> pretty @Text "Y"
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance SingSize s => Sized (Bytes s n) where
  sizeOf = Size.ssizeToSize . bytesToSSize
  {-# INLINE sizeOf #-}

-- | @since 0.1
instance Unwrapper (Bytes s n) where
  type Unwrapped (Bytes s n) = n
  unwrap (MkBytes x) = x
  {-# INLINE unwrap #-}

-- | @since 0.1
instance Read n => Parser (Bytes s n) where
  parser = MkBytes <$> (Parser.parseDigits <* MP.eof)

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

-- | @since 0.1
instance (k ~ A_Lens, a ~ m, b ~ n) => LabelOptic "unSomeSize" k (SomeSize m) (SomeSize n) a b where
  labelOptic = lens unwrap (\(MkSomeSize sz _) x -> MkSomeSize sz (MkBytes x))
  {-# INLINE labelOptic #-}

-- | @since 0.1
deriving stock instance Show n => Show (SomeSize n)

-- | @since 0.1
deriving stock instance Functor SomeSize

-- | @since 0.1
instance (Eq n, FromInteger n, MGroup n) => Eq (SomeSize n) where
  x == y = toB x == toB y
  {-# INLINE (==) #-}

-- | @since 0.1
instance (FromInteger n, MGroup n, Ord n) => Ord (SomeSize n) where
  x <= y = toB x <= toB y
  {-# INLINE (<=) #-}

-- | Fixed size 'B'.
--
-- @since 0.1
instance FromInteger n => FromInteger (SomeSize n) where
  afromInteger = MkSomeSize SB . afromInteger
  {-# INLINE afromInteger #-}

-- | Fixed size 'B'.
--
-- @since 0.1
instance FromRational n => FromRational (SomeSize n) where
  afromRational = MkSomeSize SB . afromRational
  {-# INLINE afromRational #-}

-- | @since 0.1
instance (ASemigroup n, FromInteger n, MGroup n) => ASemigroup (SomeSize n) where
  x .+. y = MkSomeSize SB $ toB x .+. toB y
  {-# INLINE (.+.) #-}

-- | @since 0.1
instance (FromInteger n, Semifield n) => AMonoid (SomeSize n) where
  zero = MkSomeSize SB zero
  {-# INLINE zero #-}

-- | @since 0.1
instance (Field n, FromInteger n) => AGroup (SomeSize n) where
  x .-. y = MkSomeSize SB $ toB x .-. toB y
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (FromInteger n, MGroup n, Normed n, Ord n) => MSemiSpace (SomeSize n) n where
  MkSomeSize sz x .* k = MkSomeSize sz $ x .* k
  {-# INLINE (.*) #-}

-- | @since 0.1
instance (FromInteger n, MGroup n, Normed n, Ord n) => MSpace (SomeSize n) n where
  MkSomeSize sz x .% k = MkSomeSize sz $ x .% k
  {-# INLINEABLE (.%) #-}

-- | @since 0.1
instance Normed n => Normed (SomeSize n) where
  norm (MkSomeSize sz x) = MkSomeSize sz (norm x)
  {-# INLINE norm #-}

-- | @since 0.1
instance (FromInteger n, Normed n, Ord n, Semifield n) => Semimodule (SomeSize n) n

-- | @since 0.1
instance (Field n, FromInteger n, Normed n, Ord n) => Module (SomeSize n) n

-- | @since 0.1
instance (FromInteger n, Normed n, Ord n, Semifield n) => SemivectorSpace (SomeSize n) n

-- | @since 0.1
instance (Field n, FromInteger n, Normed n, Ord n) => VectorSpace (SomeSize n) n

-- | @since 0.1
instance (FromInteger n, MGroup n) => Conversion (SomeSize n) where
  type Converted B (SomeSize n) = Bytes B n
  type Converted K (SomeSize n) = Bytes K n
  type Converted M (SomeSize n) = Bytes M n
  type Converted G (SomeSize n) = Bytes G n
  type Converted T (SomeSize n) = Bytes T n
  type Converted P (SomeSize n) = Bytes P n
  type Converted E (SomeSize n) = Bytes E n
  type Converted Z (SomeSize n) = Bytes Z n
  type Converted Y (SomeSize n) = Bytes Y n

  toB (MkSomeSize sz x) = Size.withSingSize sz $ toB x
  {-# INLINE toB #-}
  toK (MkSomeSize sz x) = Size.withSingSize sz $ toK x
  {-# INLINE toK #-}
  toM (MkSomeSize sz x) = Size.withSingSize sz $ toM x
  {-# INLINE toM #-}
  toG (MkSomeSize sz x) = Size.withSingSize sz $ toG x
  {-# INLINE toG #-}
  toT (MkSomeSize sz x) = Size.withSingSize sz $ toT x
  {-# INLINE toT #-}
  toP (MkSomeSize sz x) = Size.withSingSize sz $ toP x
  {-# INLINE toP #-}
  toE (MkSomeSize sz x) = Size.withSingSize sz $ toE x
  {-# INLINE toE #-}
  toZ (MkSomeSize sz x) = Size.withSingSize sz $ toZ x
  {-# INLINE toZ #-}
  toY (MkSomeSize sz x) = Size.withSingSize sz $ toY x
  {-# INLINE toY #-}

-- | @since 0.1
instance (FromInteger n, MGroup n, Normed n, Ord n) => Normalize (SomeSize n) where
  type Norm (SomeSize n) = SomeSize n
  normalize (MkSomeSize sz x) = Size.withSingSize sz $ normalize x
  {-# INLINE normalize #-}

-- | @since 0.1
instance Pretty n => Pretty (SomeSize n) where
  pretty (MkSomeSize sz b) = Size.withSingSize sz $ pretty b
  {-# INLINE pretty #-}

-- | @since 0.1
instance Sized (SomeSize n) where
  sizeOf (MkSomeSize sz _) = Size.ssizeToSize sz
  {-# INLINE sizeOf #-}

-- | @since 0.1
instance Unwrapper (SomeSize n) where
  type Unwrapped (SomeSize n) = n
  unwrap (MkSomeSize _ b) = unwrap b
  {-# INLINE unwrap #-}

-- | @since 0.1
instance Read n => Parser (SomeSize n) where
  parser = do
    bytes <- Parser.parseDigits
    MPC.space
    sz <- parser
    MPC.space
    MP.eof
    pure $ case sz of
      B -> MkSomeSize SB $ MkBytes bytes
      K -> MkSomeSize SK $ MkBytes bytes
      M -> MkSomeSize SM $ MkBytes bytes
      G -> MkSomeSize SG $ MkBytes bytes
      T -> MkSomeSize ST $ MkBytes bytes
      P -> MkSomeSize SP $ MkBytes bytes
      E -> MkSomeSize SE $ MkBytes bytes
      Z -> MkSomeSize SZ $ MkBytes bytes
      Y -> MkSomeSize SY $ MkBytes bytes
  {-# INLINEABLE parser #-}

-- | Increases 'Bytes' to the next size.
--
-- ==== __Examples__
--
-- >>> incSize $ MkBytes @M @Float 2_500
-- MkBytes 2.5
--
-- >>> -- type error: "The byte unit Y does not have a 'next size'."
-- >>> --incSize $ MkBytes @Y @Float 2_500
--
-- @since 0.1
incSize :: forall s n. (FromInteger n, MGroup n) => Bytes s n -> Bytes (NextSize s) n
incSize = resizeBytes . MkBytes . (.%. nz1000) . unwrap
  where
    nz1000 = reallyUnsafeNonZero $ afromInteger 1_000
{-# INLINE incSize #-}

-- | Decreases 'Bytes' to the previous size.
--
-- ==== __Examples__
--
-- >>> decSize $ MkBytes @M @Float 2.5
-- MkBytes 2500.0
--
-- >>> -- type error: "The byte unit B does not have a 'previous size'."
-- >>> --decSize $ MkBytes @B @Float 2.5
--
-- @since 0.1
decSize :: forall s n. (FromInteger n, MSemigroup n) => Bytes s n -> Bytes (PrevSize s) n
decSize = resizeBytes . MkBytes . (.*. afromInteger @n 1_000) . unwrap
{-# INLINE decSize #-}
