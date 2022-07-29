{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal module for "Data.Network.NetBytes". The primary
-- difference is this module exposes some underlying details i.e. singleton witnesses. These are hidden by default as they complicate the API.
--
-- @since 0.1
module Data.Bytes.Network.NetBytes.Internal
  ( -- * Network Bytes
    NetBytes (.., MkNetBytesP),
    netToSSize,
    netToSDirection,

    -- * Unknown Size
    SomeNetSize (..),
    hideNetSize,
    someNetSizeToSDirection,
  )
where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Data.Bytes.Class.Conversion (Conversion (..))
import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Class.Parser (Parser (..))
import Data.Bytes.Class.Wrapper (Unwrapper (..))
import Data.Bytes.Internal (Bytes (..), SomeSize (..))
import Data.Bytes.Network.Direction
  ( Directed (..),
    Direction (..),
    SDirection (..),
    SingDirection (..),
  )
import Data.Bytes.Network.Direction qualified as Direction
import Data.Bytes.Size (SSize (..), SingSize (..), Size (..), Sized (..))
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
import Optics.Core (A_Lens, An_Iso, LabelOptic (..), iso, lens)
#if MIN_VERSION_prettyprinter(1, 7, 1)
import Prettyprinter (Pretty (..), (<+>))
#endif

-- $setup
-- >>> getUpTrafficRaw = pure (40, "K")

-- | Wrapper around the 'Bytes' type that adds the 'Direction' tag.
--
-- ==== __Examples__
-- >>> MkNetBytesP @Up @M 1000
-- MkNetBytes (MkBytes 1000)
--
-- @since 0.1
type NetBytes :: Direction -> Size -> Type -> Type
newtype NetBytes (d :: Direction) (s :: Size) (n :: Type) = MkNetBytes (Bytes s n)
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

-- | Pattern for de/constructing 'NetBytes'.
--
-- @since 0.1
pattern MkNetBytesP :: forall d s n. n -> NetBytes d s n
pattern MkNetBytesP x <-
  MkNetBytes (MkBytes x)
  where
    MkNetBytesP x = MkNetBytes (MkBytes x)

{-# COMPLETE MkNetBytesP #-}

-- | Retrieves the 'SDirection' witness. Can be used to recover the
-- 'Direction'.
--
-- >>> netToSDirection (MkNetBytesP @Up @K @Int 7)
-- SUp
--
-- @since 0.1
netToSDirection :: SingDirection d => NetBytes d s n -> SDirection d
netToSDirection _ = singDirection
{-# INLINE netToSDirection #-}

-- | Retrieves the 'SingSize' witness. Can be used to recover the 'Size'.
--
-- >>> netToSSize (MkNetBytesP @Down @K @Int 7)
-- SK
--
-- @since 0.1
netToSSize :: SingSize s => NetBytes d s n -> SSize s
netToSSize _ = singSize
{-# INLINE netToSSize #-}

-- | @since 0.1
instance (k ~ An_Iso, a ~ m, b ~ n) => LabelOptic "unNetBytes" k (NetBytes d s m) (NetBytes d s n) a b where
  labelOptic = iso unwrap (MkNetBytes . MkBytes)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance Applicative (NetBytes d s) where
  pure = MkNetBytes . pure
  {-# INLINE pure #-}
  MkNetBytes f <*> MkNetBytes x = MkNetBytes $ f <*> x
  {-# INLINE (<*>) #-}

-- | @since 0.1
instance Monad (NetBytes d s) where
  MkNetBytes x >>= f = MkNetBytes $ x >>= (unNetBytes . f)
  {-# INLINE (>>=) #-}

-- | @since 0.1
instance ASemigroup n => ASemigroup (NetBytes d s n) where
  (.+.) = liftA2 (.+.)
  {-# INLINE (.+.) #-}

-- | @since 0.1
instance AMonoid n => AMonoid (NetBytes d s n) where
  zero = MkNetBytes zero
  {-# INLINE zero #-}

-- | @since 0.1
instance AGroup n => AGroup (NetBytes d s n) where
  (.-.) = liftA2 (.-.)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance Normed n => Normed (NetBytes d s n) where
  norm (MkNetBytes b) = MkNetBytes (norm b)
  {-# INLINE norm #-}

-- | @since 0.1
instance MSemigroup n => MSemiSpace (NetBytes d s n) n where
  MkNetBytes x .* k = MkNetBytes $ x .* k
  {-# INLINE (.*) #-}

-- | @since 0.1
instance MGroup n => MSpace (NetBytes d s n) n where
  MkNetBytes x .% k = MkNetBytes $ x .% k
  {-# INLINEABLE (.%) #-}

-- | @since 0.1
instance Semiring n => Semimodule (NetBytes d s n) n

-- | @since 0.1
instance Ring n => Module (NetBytes d s n) n

-- | @since 0.1
instance Semifield n => SemivectorSpace (NetBytes d s n) n

-- | @since 0.1
instance Field n => VectorSpace (NetBytes d s n) n

-- | @since 0.1
instance (MGroup n, NumLiteral n, SingSize s) => Conversion (NetBytes d s n) where
  type Converted B (NetBytes d s n) = NetBytes d B n
  type Converted K (NetBytes d s n) = NetBytes d K n
  type Converted M (NetBytes d s n) = NetBytes d M n
  type Converted G (NetBytes d s n) = NetBytes d G n
  type Converted T (NetBytes d s n) = NetBytes d T n
  type Converted P (NetBytes d s n) = NetBytes d P n
  type Converted E (NetBytes d s n) = NetBytes d E n
  type Converted Z (NetBytes d s n) = NetBytes d Z n
  type Converted Y (NetBytes d s n) = NetBytes d Y n

  toB (MkNetBytes b) = MkNetBytes $ toB b
  {-# INLINE toB #-}
  toK (MkNetBytes b) = MkNetBytes $ toK b
  {-# INLINE toK #-}
  toM (MkNetBytes b) = MkNetBytes $ toM b
  {-# INLINE toM #-}
  toG (MkNetBytes b) = MkNetBytes $ toG b
  {-# INLINE toG #-}
  toT (MkNetBytes b) = MkNetBytes $ toT b
  {-# INLINE toT #-}
  toP (MkNetBytes b) = MkNetBytes $ toP b
  {-# INLINE toP #-}
  toE (MkNetBytes b) = MkNetBytes $ toE b
  {-# INLINE toE #-}
  toZ (MkNetBytes b) = MkNetBytes $ toZ b
  {-# INLINE toZ #-}
  toY (MkNetBytes b) = MkNetBytes $ toY b
  {-# INLINE toY #-}

-- | @since 0.1
instance (MGroup n, Normed n, NumLiteral n, Ord n, SingSize s) => Normalize (NetBytes d s n) where
  type Norm (NetBytes d s n) = SomeNetSize d n

  normalize (MkNetBytes bytes) = case normalize bytes of
    MkSomeSize sz bytes' -> MkSomeNetSize sz $ MkNetBytes bytes'
  {-# INLINE normalize #-}

-- | @since 0.1
instance
  forall d s n.
  (Pretty n, SingDirection d, SingSize s) =>
  Pretty (NetBytes d s n)
  where
  pretty (MkNetBytes x) = case singDirection @d of
    SDown -> pretty x <+> pretty @String "Down"
    SUp -> pretty x <+> pretty @String "Up"
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance SingSize s => Sized (NetBytes d s n) where
  sizeOf = Size.ssizeToSize . netToSSize
  {-# INLINE sizeOf #-}

-- | @since 0.1
instance SingDirection d => Directed (NetBytes d s n) where
  directionOf = Direction.sdirectionToDirection . netToSDirection
  {-# INLINE directionOf #-}

-- | @since 0.1
instance Unwrapper (NetBytes d s n) where
  type Unwrapped (NetBytes d s n) = n
  unwrap (MkNetBytes b) = unwrap b
  {-# INLINE unwrap #-}

-- | @since 0.1
instance Read n => Parser (NetBytes d s n) where
  parser = MkNetBytes <$> parser
  {-# INLINE parser #-}

-- | Wrapper for 'NetBytes', existentially quantifying the size. This is useful
-- when a function does not know a priori what size it should return e.g.
--
-- >>> :{
--   getUpTraffic :: IO (SomeNetSize Up Float)
--   getUpTraffic = do
--     -- getUpTrafficRaw :: IO (Float, String)
--     (bytes, units) <- getUpTrafficRaw
--     pure $ case units of
--       "B" -> hideNetSize $ MkNetBytesP @Up @B bytes
--       "K" -> hideNetSize $ MkNetBytesP @Up @K bytes
--       _ -> error "todo"
-- :}
--
-- 'SomeNetSize' carries along an 'SSize' runtime witness for when we
-- need the size. Its 'Numeric.Algebra' functions are 'normalize'd.
--
-- We define an equivalence relation on 'SomeNetSize' that takes units into
-- account. For instance,
--
-- >>> hideNetSize (MkNetBytesP @Up @K 1000) == hideNetSize (MkNetBytesP @Up @M 1)
-- True
--
-- Because we expose the underlying @NetBytes@ in several ways (e.g. 'Show',
-- the 'SSize' witness), this is technically unlawful for equality
-- as it breaks the extensionality law:
--
-- \[
-- x = y \implies f(x) = f(y).
-- \]
--
-- @since 0.1
type SomeNetSize :: Direction -> Type -> Type
data SomeNetSize (d :: Direction) (n :: Type) where
  -- | @since 0.1
  MkSomeNetSize :: SSize s -> NetBytes d s n -> SomeNetSize d n

-- | Wraps a 'NetBytes' in an existentially quantified 'SomeNetSize'.
--
-- @since 0.1
hideNetSize :: forall d s n. SingSize s => NetBytes d s n -> SomeNetSize d n
hideNetSize bytes = case singSize @s of
  SB -> MkSomeNetSize SB bytes
  SK -> MkSomeNetSize SK bytes
  SM -> MkSomeNetSize SM bytes
  SG -> MkSomeNetSize SG bytes
  ST -> MkSomeNetSize ST bytes
  SP -> MkSomeNetSize SP bytes
  SE -> MkSomeNetSize SE bytes
  SZ -> MkSomeNetSize SZ bytes
  SY -> MkSomeNetSize SY bytes
{-# INLINEABLE hideNetSize #-}

-- | @since 0.1
instance (k ~ A_Lens, a ~ m, b ~ n) => LabelOptic "unSomeNetSize" k (SomeNetSize d m) (SomeNetSize d n) a b where
  labelOptic = lens unwrap (\(MkSomeNetSize sz _) x -> MkSomeNetSize sz (MkNetBytesP x))
  {-# INLINE labelOptic #-}

-- | @since 0.1
deriving stock instance Show n => Show (SomeNetSize d n)

-- | @since 0.1
deriving stock instance Functor (SomeNetSize d)

-- | @since 0.1
instance (MGroup n, Eq n, NumLiteral n) => Eq (SomeNetSize d n) where
  x == y = toB x == toB y
  {-# INLINE (==) #-}

-- | @since 0.1
instance (MGroup n, NumLiteral n, Ord n) => Ord (SomeNetSize d n) where
  x <= y = toB x <= toB y
  {-# INLINE (<=) #-}

-- | @since 0.1
instance (ASemigroup n, MGroup n, NumLiteral n) => ASemigroup (SomeNetSize d n) where
  x .+. y = MkSomeNetSize SB $ toB x .+. toB y
  {-# INLINE (.+.) #-}

-- | @since 0.1
instance (NumLiteral n, Semifield n) => AMonoid (SomeNetSize d n) where
  zero = MkSomeNetSize SB zero
  {-# INLINE zero #-}

-- | @since 0.1
instance (Field n, NumLiteral n) => AGroup (SomeNetSize d n) where
  x .-. y = MkSomeNetSize SB $ toB x .-. toB y
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (MGroup n, Normed n, NumLiteral n, Ord n) => MSemiSpace (SomeNetSize d n) n where
  MkSomeNetSize sz x .* k = MkSomeNetSize sz $ x .* k
  {-# INLINE (.*) #-}

-- | @since 0.1
instance (MGroup n, Normed n, NumLiteral n, Ord n) => MSpace (SomeNetSize d n) n where
  MkSomeNetSize sz x .% k = MkSomeNetSize sz $ x .% k
  {-# INLINEABLE (.%) #-}

-- | @since 0.1
instance (Normed n, NumLiteral n, Ord n, Semifield n) => Semimodule (SomeNetSize d n) n

-- | @since 0.1
instance (Field n, Normed n, NumLiteral n, Ord n) => Module (SomeNetSize d n) n

-- | @since 0.1
instance (Normed n, NumLiteral n, Ord n, Semifield n) => SemivectorSpace (SomeNetSize d n) n

-- | @since 0.1
instance (Field n, Normed n, NumLiteral n, Ord n) => VectorSpace (SomeNetSize d n) n

-- | @since 0.1
instance (MGroup n, NumLiteral n) => Conversion (SomeNetSize d n) where
  type Converted B (SomeNetSize d n) = NetBytes d B n
  type Converted K (SomeNetSize d n) = NetBytes d K n
  type Converted M (SomeNetSize d n) = NetBytes d M n
  type Converted G (SomeNetSize d n) = NetBytes d G n
  type Converted T (SomeNetSize d n) = NetBytes d T n
  type Converted P (SomeNetSize d n) = NetBytes d P n
  type Converted E (SomeNetSize d n) = NetBytes d E n
  type Converted Z (SomeNetSize d n) = NetBytes d Z n
  type Converted Y (SomeNetSize d n) = NetBytes d Y n

  toB (MkSomeNetSize sz x) = Size.withSingSize sz $ toB x
  {-# INLINE toB #-}
  toK (MkSomeNetSize sz x) = Size.withSingSize sz $ toK x
  {-# INLINE toK #-}
  toM (MkSomeNetSize sz x) = Size.withSingSize sz $ toM x
  {-# INLINE toM #-}
  toG (MkSomeNetSize sz x) = Size.withSingSize sz $ toG x
  {-# INLINE toG #-}
  toT (MkSomeNetSize sz x) = Size.withSingSize sz $ toT x
  {-# INLINE toT #-}
  toP (MkSomeNetSize sz x) = Size.withSingSize sz $ toP x
  {-# INLINE toP #-}
  toE (MkSomeNetSize sz x) = Size.withSingSize sz $ toE x
  {-# INLINE toE #-}
  toZ (MkSomeNetSize sz x) = Size.withSingSize sz $ toZ x
  {-# INLINE toZ #-}
  toY (MkSomeNetSize sz x) = Size.withSingSize sz $ toY x
  {-# INLINE toY #-}

-- | @since 0.1
instance (MGroup n, Normed n, NumLiteral n, Ord n) => Normalize (SomeNetSize d n) where
  type Norm (SomeNetSize d n) = SomeNetSize d n
  normalize (MkSomeNetSize sz x) = Size.withSingSize sz $ normalize x
  {-# INLINE normalize #-}

-- | @since 0.1
instance (Pretty n, SingDirection d) => Pretty (SomeNetSize d n) where
  pretty (MkSomeNetSize sz b) = Size.withSingSize sz $ pretty b
  {-# INLINE pretty #-}

-- | @since 0.1
instance Sized (SomeNetSize d n) where
  sizeOf (MkSomeNetSize sz _) = Size.ssizeToSize sz
  {-# INLINE sizeOf #-}

-- | @since 0.1
instance SingDirection d => Directed (SomeNetSize d n) where
  directionOf = Direction.sdirectionToDirection . someNetSizeToSDirection
  {-# INLINE directionOf #-}

-- | @since 0.1
instance Unwrapper (SomeNetSize d n) where
  type Unwrapped (SomeNetSize d n) = n
  unwrap (MkSomeNetSize _ b) = unwrap b
  {-# INLINE unwrap #-}

-- | @since 0.1
instance Read n => Parser (SomeNetSize d n) where
  parser = do
    MkSomeSize sz bytes <- parser
    pure $ MkSomeNetSize sz (MkNetBytes bytes)

-- | Retrieves the 'SingDirection' witness. Can be used to recover the
-- 'Direction'.
--
-- @since 0.1
someNetSizeToSDirection :: SingDirection d => SomeNetSize d n -> SDirection d
someNetSizeToSDirection _ = singDirection
{-# INLINE someNetSizeToSDirection #-}

unNetBytes :: NetBytes d s n -> Bytes s n
unNetBytes (MkNetBytes x) = x
