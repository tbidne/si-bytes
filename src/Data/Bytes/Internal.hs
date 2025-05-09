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
    _MkBytes,
    bytesToSSize,

    -- * Unknown Size
    SomeSize (..),
    _MkSomeSize,
  )
where

#if !MIN_VERSION_base(4, 18, 0)
import Control.Applicative (liftA2)
#endif
import Control.DeepSeq (NFData (rnf), deepseq)
import Data.Bounds
  ( LowerBounded,
    LowerBoundless,
    MaybeLowerBounded,
    MaybeUpperBounded,
    UpperBounded,
    UpperBoundless,
  )
import Data.Bytes.Class.Conversion (Conversion (convert_))
import Data.Bytes.Class.Conversion qualified as Conv
import Data.Bytes.Class.Normalize (Normalize (Norm, normalize))
import Data.Bytes.Class.Parser (Parser (parser))
import Data.Bytes.Class.Parser qualified as Parser
import Data.Bytes.Class.RawNumeric (RawNumeric (Raw, toRaw))
import Data.Bytes.Size
  ( NextSize,
    PrevSize,
    SSize (SB, SE, SG, SK, SM, SP, ST, SY, SZ),
    Size (B, E, G, K, M, P, T, Y, Z),
    Sized (hideSize),
  )
import Data.Bytes.Size qualified as Size
import Data.Hashable as X (Hashable (hashWithSalt))
import Data.Kind (Type)
import Data.Singletons (SingI)
import Data.Singletons qualified as Sing
import GHC.Generics (Generic)
#if MIN_VERSION_base(4, 16, 0)
import GHC.Records (HasField (getField))
#endif
import Numeric.Algebra
  ( AGroup ((.-.)),
    AMonoid (zero),
    ASemigroup ((.+.)),
    Field,
    MGroup ((.%.)),
    MSemiSpace ((.*)),
    MSemigroup ((.*.)),
    MSpace ((.%)),
    MetricSpace (diffR),
    Module,
    Normed (norm, sgn),
    Ring,
    Semifield,
    Semimodule,
    Semiring,
    SemivectorSpace,
    VectorSpace,
  )
import Numeric.Convert.Integer (FromInteger (fromZ), ToInteger (toZ))
import Numeric.Convert.Rational (FromRational (fromQ), ToRational (toQ))
import Numeric.Convert.Real (FromReal (fromR), ToReal (toR))
import Optics.Core (A_Getter, An_Iso, Iso', LabelOptic (labelOptic), iso, to)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- $setup
-- >>> import Data.Bytes.Size (Size (..), Sized (..))
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
      Bounded,
      -- | @since 0.1
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
  deriving
    ( -- | @since 0.1
      Hashable,
      -- | @since 0.1
      LowerBounded,
      -- | @since 0.1
      LowerBoundless,
      -- | @since 0.1
      MaybeLowerBounded,
      -- | @since 0.1
      MaybeUpperBounded,
      -- | @since 0.1
      UpperBounded,
      -- | @since 0.1
      UpperBoundless
    )
    via n

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
bytesToSSize :: (SingI s) => Bytes s n -> SSize s
bytesToSSize _ = Sing.sing
{-# INLINE bytesToSSize #-}

-- | 'Iso'' between 'Bytes' and underlying value.
--
-- ==== __Examples__
--
-- >>> import Optics.Core (review, view)
-- >>> (review _MkBytes 70) :: Bytes K Int
-- MkBytes 70
--
-- >>> view _MkBytes (MkBytes @K @Int 70)
-- 70
--
-- @since 0.1
_MkBytes :: forall s n. Iso' (Bytes s n) n
_MkBytes = iso toRaw MkBytes
{-# INLINE _MkBytes #-}

#if MIN_VERSION_base(4, 16, 0)

-- | @since 0.1
instance HasField "unBytes" (Bytes s n) n where
  getField (MkBytes x) = x

#endif

-- | @since 0.1
instance
  ( k ~ An_Iso,
    a ~ n,
    b ~ n
  ) =>
  LabelOptic "unBytes" k (Bytes s n) (Bytes s n) a b
  where
  labelOptic = iso (\(MkBytes x) -> x) MkBytes
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
instance Foldable (Bytes s) where
  foldr f e (MkBytes x) = f x e
  {-# INLINE foldr #-}

-- | @since 0.1
instance Traversable (Bytes s) where
  traverse f (MkBytes x) = MkBytes <$> f x
  {-# INLINE traverse #-}

  sequenceA (MkBytes x) = MkBytes <$> x
  {-# INLINE sequenceA #-}

-- | @since 0.1
instance (FromInteger n) => FromInteger (Bytes s n) where
  fromZ = MkBytes . fromZ
  {-# INLINE fromZ #-}

-- | @since 0.1
instance (ToInteger n) => ToInteger (Bytes s n) where
  toZ (MkBytes x) = toZ x
  {-# INLINE toZ #-}

-- | @since 0.1
instance (FromRational n) => FromRational (Bytes s n) where
  fromQ = MkBytes . fromQ
  {-# INLINE fromQ #-}

-- | @since 0.1
instance (ToRational n) => ToRational (Bytes s n) where
  toQ (MkBytes x) = toQ x
  {-# INLINE toQ #-}

-- | @since 0.1
instance (FromReal n) => FromReal (Bytes s n) where
  fromR = MkBytes . fromR
  {-# INLINE fromR #-}

-- | @since 0.1
instance (ToReal n) => ToReal (Bytes s n) where
  toR (MkBytes x) = toR x
  {-# INLINE toR #-}

-- | @since 0.1
instance (ASemigroup n) => ASemigroup (Bytes s n) where
  (.+.) = liftA2 (.+.)
  {-# INLINE (.+.) #-}

-- | @since 0.1
instance (AMonoid n) => AMonoid (Bytes s n) where
  zero = MkBytes zero
  {-# INLINE zero #-}

-- | @since 0.1
instance (AGroup n) => AGroup (Bytes s n) where
  (.-.) = liftA2 (.-.)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (MSemigroup n) => MSemiSpace (Bytes s n) n where
  MkBytes x .* k = MkBytes $ x .*. k
  {-# INLINE (.*) #-}

-- | @since 0.1
instance (MGroup n) => MSpace (Bytes s n) n where
  MkBytes x .% k = MkBytes $ x .%. k
  {-# INLINEABLE (.%) #-}

-- | @since 0.1
instance (Normed n) => Normed (Bytes s n) where
  norm (MkBytes x) = MkBytes (norm x)
  {-# INLINE norm #-}

  sgn (MkBytes x) = MkBytes (sgn x)
  {-# INLINE sgn #-}

-- | @since 0.1
instance (Semiring n) => Semimodule (Bytes s n) n

-- | @since 0.1
instance (Ring n) => Module (Bytes s n) n

-- | @since 0.1
instance (Semifield n) => SemivectorSpace (Bytes s n) n

-- | @since 0.1
instance (Field n) => VectorSpace (Bytes s n) n

-- | @since 0.1
instance (MetricSpace n) => MetricSpace (Bytes s n) where
  diffR (MkBytes x) (MkBytes y) = x `diffR` y

-- | @since 0.1
instance (FromInteger n, MGroup n, SingI s) => Conversion (Bytes s n) where
  type Converted t (Bytes s n) = Bytes t n

  convert_ :: forall t. (SingI t) => Bytes s n -> Bytes t n
  convert_ (MkBytes x) = MkBytes $ Conv.convertWitness @s (Sing.fromSing $ Sing.sing @t) x

-- | @since 0.1
instance
  forall n s.
  (FromInteger n, MGroup n, Normed n, Ord n, SingI s) =>
  Normalize (Bytes s n)
  where
  type Norm (Bytes s n) = SomeSize n

  normalize bytes@(MkBytes x) =
    case sz of
      SB
        | tooLarge -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
      SY
        | tooSmall -> normalize $ decSize bytes
        | otherwise -> MkSomeSize sz bytes
      SK
        | tooSmall -> normalize $ decSize bytes
        | tooLarge -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
      SM
        | tooSmall -> normalize $ decSize bytes
        | tooLarge -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
      SG
        | tooSmall -> normalize $ decSize bytes
        | tooLarge -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
      ST
        | tooSmall -> normalize $ decSize bytes
        | tooLarge -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
      SP
        | tooSmall -> normalize $ decSize bytes
        | tooLarge -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
      SE
        | tooSmall -> normalize $ decSize bytes
        | tooLarge -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
      SZ
        | tooSmall -> normalize $ decSize bytes
        | tooLarge -> normalize $ incSize bytes
        | otherwise -> MkSomeSize sz bytes
    where
      absBytes = norm x
      tooSmall = absBytes < fromZ 1
      tooLarge = absBytes >= fromZ 1_000
      sz = bytesToSSize bytes
  {-# INLINEABLE normalize #-}

-- | @since 0.1
instance (SingI s) => Sized (Bytes s n) where
  type HideSize (Bytes s n) = SomeSize n

  sizeOf = Sing.fromSing . bytesToSSize
  {-# INLINE sizeOf #-}

  hideSize b@(MkBytes _) = MkSomeSize (Sing.sing @s) b
  {-# INLINE hideSize #-}

-- | @since 0.1
instance RawNumeric (Bytes s n) where
  type Raw (Bytes s n) = n
  toRaw (MkBytes x) = x
  {-# INLINE toRaw #-}

-- | @since 0.1
instance (Read n) => Parser (Bytes s n) where
  parser = MkBytes <$> (Parser.parseDigits <* (MPC.space *> MP.eof))

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

-- | 'Iso'' between 'SomeSize' and underlying 'Bytes'. Performs any necessary
-- conversions when going from @SomeSize n -> Bytes s n@.
--
-- ==== __Examples__
--
-- >>> import Optics.Core (review, view)
-- >>> review _MkSomeSize (MkBytes @K @Int 70)
-- MkSomeSize SK (MkBytes 70)
--
-- >>> (view _MkSomeSize (hideSize $ MkBytes @K @Int 70)) :: Bytes B Int
-- MkBytes 70000
--
-- @since 0.1
_MkSomeSize :: forall s n. (FromInteger n, MGroup n, SingI s) => Iso' (SomeSize n) (Bytes s n)
_MkSomeSize = iso convert_ hideSize
{-# INLINE _MkSomeSize #-}

-- | @since 0.1
deriving stock instance (Show n) => Show (SomeSize n)

-- | @since 0.1
deriving stock instance Functor SomeSize

-- | @since 0.1
instance Foldable SomeSize where
  foldr f e (MkSomeSize _ (MkBytes x)) = f x e
  {-# INLINE foldr #-}

-- | @since 0.1
instance Traversable SomeSize where
  traverse f (MkSomeSize sz (MkBytes x)) = MkSomeSize sz . MkBytes <$> f x
  {-# INLINE traverse #-}

  sequenceA (MkSomeSize sz (MkBytes x)) = MkSomeSize sz . MkBytes <$> x
  {-# INLINE sequenceA #-}

#if MIN_VERSION_base(4, 16, 0)

-- | @since 0.1
instance HasField "unSomeSize" (SomeSize n) n where
  getField (MkSomeSize _ (MkBytes x)) = x

#endif

-- | @since 0.1
instance
  ( k ~ A_Getter,
    a ~ n,
    b ~ n
  ) =>
  LabelOptic "unSomeSize" k (SomeSize n) (SomeSize n) a b
  where
  labelOptic = to (\(MkSomeSize _ (MkBytes x)) -> x)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance (FromInteger n, Hashable n, MGroup n) => Hashable (SomeSize n) where
  hashWithSalt i (MkSomeSize sz x) =
    i `hashWithSalt` Sing.fromSing sz `hashWithSalt` x

-- | @since 0.1
instance (NFData n) => NFData (SomeSize n) where
  rnf (MkSomeSize sz x) = sz `deepseq` x `deepseq` ()

-- | @since 0.1
instance (Eq n, FromInteger n, MGroup n) => Eq (SomeSize n) where
  x == y = convert_ @_ @B x == convert_ y
  {-# INLINE (==) #-}

-- | @since 0.1
instance (FromInteger n, MGroup n, Ord n) => Ord (SomeSize n) where
  x <= y = convert_ @_ @B x <= convert_ y
  {-# INLINE (<=) #-}

-- | Fixed size 'B'.
--
-- @since 0.1
instance (FromInteger n) => FromInteger (SomeSize n) where
  fromZ = MkSomeSize SB . fromZ
  {-# INLINE fromZ #-}

-- | @since 0.1
instance (FromInteger n, MGroup n, ToInteger n) => ToInteger (SomeSize n) where
  toZ = toZ . convert_ @_ @B
  {-# INLINE toZ #-}

-- | Fixed size 'B'.
--
-- @since 0.1
instance (FromRational n) => FromRational (SomeSize n) where
  fromQ = MkSomeSize SB . fromQ
  {-# INLINE fromQ #-}

-- | @since 0.1
instance (FromInteger n, MGroup n, ToRational n) => ToRational (SomeSize n) where
  toQ = toQ . convert_ @_ @B
  {-# INLINE toQ #-}

-- | @since 0.1
instance (FromReal n) => FromReal (SomeSize n) where
  fromR = MkSomeSize SB . fromR
  {-# INLINE fromR #-}

-- | @since 0.1
instance (FromInteger n, MGroup n, ToReal n) => ToReal (SomeSize n) where
  toR = toR . convert_ @_ @B
  {-# INLINE toR #-}

-- | @since 0.1
instance (ASemigroup n, FromInteger n, MGroup n) => ASemigroup (SomeSize n) where
  x .+. y = MkSomeSize SB $ convert_ x .+. convert_ y
  {-# INLINE (.+.) #-}

-- | @since 0.1
instance (FromInteger n, Semifield n) => AMonoid (SomeSize n) where
  zero = MkSomeSize SB zero
  {-# INLINE zero #-}

-- | @since 0.1
instance (Field n, FromInteger n) => AGroup (SomeSize n) where
  x .-. y = MkSomeSize SB $ convert_ x .-. convert_ y
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (MGroup n) => MSemiSpace (SomeSize n) n where
  MkSomeSize sz x .* k = MkSomeSize sz $ x .* k
  {-# INLINE (.*) #-}

-- | @since 0.1
instance (MGroup n) => MSpace (SomeSize n) n where
  MkSomeSize sz x .% k = MkSomeSize sz $ x .% k
  {-# INLINEABLE (.%) #-}

-- | @since 0.1
instance (Normed n) => Normed (SomeSize n) where
  norm (MkSomeSize sz x) = MkSomeSize sz (norm x)
  {-# INLINE norm #-}

  sgn (MkSomeSize sz x) = MkSomeSize sz (sgn x)
  {-# INLINE sgn #-}

-- | @since 0.1
instance (FromInteger n, Semifield n) => Semimodule (SomeSize n) n

-- | @since 0.1
instance (Field n, FromInteger n) => Module (SomeSize n) n

-- | @since 0.1
instance (FromInteger n, Semifield n) => SemivectorSpace (SomeSize n) n

-- | @since 0.1
instance (Field n, FromInteger n) => VectorSpace (SomeSize n) n

-- | @since 0.1
instance (FromInteger n, MetricSpace n, MGroup n) => MetricSpace (SomeSize n) where
  diffR x y = convert_ @_ @B x `diffR` convert_ @_ @B y

-- | @since 0.1
instance (FromInteger n, MGroup n) => Conversion (SomeSize n) where
  type Converted t (SomeSize n) = Bytes t n

  convert_ :: forall t. (SingI t) => SomeSize n -> Bytes t n
  convert_ (MkSomeSize sz x) = Sing.withSingI sz $ convert_ x

-- | @since 0.1
instance (FromInteger n, MGroup n, Normed n, Ord n) => Normalize (SomeSize n) where
  type Norm (SomeSize n) = SomeSize n
  normalize (MkSomeSize sz x) = Sing.withSingI sz $ normalize x
  {-# INLINE normalize #-}

-- | @since 0.1
instance Sized (SomeSize n) where
  type HideSize (SomeSize n) = SomeSize n

  sizeOf (MkSomeSize sz _) = Sing.fromSing sz
  {-# INLINE sizeOf #-}

  hideSize = id
  {-# INLINE hideSize #-}

-- | @since 0.1
instance RawNumeric (SomeSize n) where
  type Raw (SomeSize n) = n
  toRaw (MkSomeSize _ b) = toRaw b
  {-# INLINE toRaw #-}

-- | @since 0.1
instance (Read n) => Parser (SomeSize n) where
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
-- >> incSize $ MkBytes @M @Float 2_500
-- MkBytes 2.5
--
-- >> -- type error: "The byte unit Y does not have a 'next size'."
-- >> --incSize $ MkBytes @Y @Float 2_500
--
-- @since 0.1
incSize :: forall s n. (FromInteger n, MGroup n) => Bytes s n -> Bytes (NextSize s) n
incSize = resizeBytes . MkBytes . (.%. nz1000) . toRaw
  where
    nz1000 = fromZ 1_000
{-# INLINE incSize #-}

-- | Decreases 'Bytes' to the previous size.
--
-- ==== __Examples__
--
-- >> decSize $ MkBytes @M @Float 2.5
-- MkBytes 2500.0
--
-- >> -- type error: "The byte unit B does not have a 'previous size'."
-- >> --decSize $ MkBytes @B @Float 2.5
--
-- @since 0.1
decSize :: forall s n. (FromInteger n, MSemigroup n) => Bytes s n -> Bytes (PrevSize s) n
decSize = resizeBytes . MkBytes . (.*. fromZ @n 1_000) . toRaw
{-# INLINE decSize #-}
