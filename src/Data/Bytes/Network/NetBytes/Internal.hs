{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal module for "Data.Network.NetBytes". The primary
-- difference is this module exposes some underlying details i.e. singleton witnesses. These are hidden by default as they complicate the API.
--
-- @since 0.1
module Data.Bytes.Network.NetBytes.Internal
  ( -- * Network Bytes
    NetBytes (.., MkNetBytesP),
    _MkNetBytes,
    netToSSize,
    netToSDirection,

    -- * Unknown Size
    SomeNetSize (..),
    _MkSomeNetSize,
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
import Data.Proxy (Proxy (..))
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
import Numeric.Literal.Integer (FromInteger (..))
import Numeric.Literal.Rational (FromRational (..))
import Optics.Core (A_Lens, Iso', LabelOptic (..), iso, lens)
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
_MkNetBytes :: Iso' (NetBytes d s n) (Bytes s n)
_MkNetBytes = iso (\(MkNetBytes x) -> x) MkNetBytes
{-# INLINE _MkNetBytes #-}

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
instance FromInteger n => FromInteger (NetBytes d s n) where
  afromInteger = MkNetBytes . afromInteger
  {-# INLINE afromInteger #-}

-- | @since 0.1
instance FromRational n => FromRational (NetBytes d s n) where
  afromRational = MkNetBytes . afromRational
  {-# INLINE afromRational #-}

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
instance (FromInteger n, MGroup n, SingSize s) => Conversion (NetBytes d s n) where
  type Converted t (NetBytes d s n) = NetBytes d t n

  convert :: forall t. SingSize t => Proxy t -> NetBytes d s n -> NetBytes d t n
  convert proxy (MkNetBytes x) = MkNetBytes $ convert proxy x

-- | @since 0.1
instance (FromInteger n, MGroup n, Normed n, Ord n, SingSize s) => Normalize (NetBytes d s n) where
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
  type HideSize (NetBytes d s n) = SomeNetSize d n

  sizeOf = Size.ssizeToSize . netToSSize
  {-# INLINE sizeOf #-}

  hideSize b@(MkNetBytes _) = MkSomeNetSize (singSize @s) b
  {-# INLINE hideSize #-}

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
--       "B" -> hideSize $ MkNetBytesP @Up @B bytes
--       "K" -> hideSize $ MkNetBytesP @Up @K bytes
--       _ -> error "todo"
-- :}
--
-- 'SomeNetSize' carries along an 'SSize' runtime witness for when we
-- need the size. Its 'Numeric.Algebra' functions are 'normalize'd.
--
-- We define an equivalence relation on 'SomeNetSize' that takes units into
-- account. For instance,
--
-- >>> hideSize (MkNetBytesP @Up @K 1000) == hideSize (MkNetBytesP @Up @M 1)
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

-- | @since 0.1
instance (k ~ A_Lens, a ~ m, b ~ n) => LabelOptic "unSomeNetSize" k (SomeNetSize d m) (SomeNetSize d n) a b where
  labelOptic = lens unwrap (\(MkSomeNetSize sz _) x -> MkSomeNetSize sz (MkNetBytesP x))
  {-# INLINE labelOptic #-}

-- | 'Iso' between 'SomeNetSize' and underlying 'NetBytes'. Performs any
-- necessary conversions when going from @SomeNetSize d n -> NetBytes d s n@.
--
-- @since 0.1
_MkSomeNetSize :: (FromInteger n, MGroup n, SingSize s) => Iso' (SomeNetSize d n) (NetBytes d s n)
_MkSomeNetSize = iso (convert Proxy) hideSize
{-# INLINE _MkSomeNetSize #-}

-- | @since 0.1
deriving stock instance Show n => Show (SomeNetSize d n)

-- | @since 0.1
deriving stock instance Functor (SomeNetSize d)

-- | @since 0.1
instance (Eq n, FromInteger n, MGroup n) => Eq (SomeNetSize d n) where
  x == y = convert @_ @B Proxy x == convert Proxy y
  {-# INLINE (==) #-}

-- | @since 0.1
instance (FromInteger n, MGroup n, Ord n) => Ord (SomeNetSize d n) where
  x <= y = convert @_ @B Proxy x <= convert Proxy y
  {-# INLINE (<=) #-}

-- | Fixed size 'B'.
--
-- @since 0.1
instance FromInteger n => FromInteger (SomeNetSize d n) where
  afromInteger = MkSomeNetSize SB . afromInteger
  {-# INLINE afromInteger #-}

-- | Fixed size 'B'.
--
-- @since 0.1
instance FromRational n => FromRational (SomeNetSize d n) where
  afromRational = MkSomeNetSize SB . afromRational
  {-# INLINE afromRational #-}

-- | @since 0.1
instance (ASemigroup n, FromInteger n, MGroup n) => ASemigroup (SomeNetSize d n) where
  x .+. y = MkSomeNetSize SB $ convert Proxy x .+. convert Proxy y
  {-# INLINE (.+.) #-}

-- | @since 0.1
instance (FromInteger n, Semifield n) => AMonoid (SomeNetSize d n) where
  zero = MkSomeNetSize SB zero
  {-# INLINE zero #-}

-- | @since 0.1
instance (Field n, FromInteger n) => AGroup (SomeNetSize d n) where
  x .-. y = MkSomeNetSize SB $ convert Proxy x .-. convert Proxy y
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (FromInteger n, MGroup n, Normed n, Ord n) => MSemiSpace (SomeNetSize d n) n where
  MkSomeNetSize sz x .* k = MkSomeNetSize sz $ x .* k
  {-# INLINE (.*) #-}

-- | @since 0.1
instance (FromInteger n, MGroup n, Normed n, Ord n) => MSpace (SomeNetSize d n) n where
  MkSomeNetSize sz x .% k = MkSomeNetSize sz $ x .% k
  {-# INLINEABLE (.%) #-}

-- | @since 0.1
instance (FromInteger n, Normed n, Ord n, Semifield n) => Semimodule (SomeNetSize d n) n

-- | @since 0.1
instance (Field n, FromInteger n, Normed n, Ord n) => Module (SomeNetSize d n) n

-- | @since 0.1
instance (FromInteger n, Normed n, Ord n, Semifield n) => SemivectorSpace (SomeNetSize d n) n

-- | @since 0.1
instance (FromInteger n, Field n, Normed n, Ord n) => VectorSpace (SomeNetSize d n) n

-- | @since 0.1
instance (FromInteger n, MGroup n) => Conversion (SomeNetSize d n) where
  type Converted t (SomeNetSize d n) = NetBytes d t n

  convert :: forall t. SingSize t => Proxy t -> SomeNetSize d n -> NetBytes d t n
  convert proxy (MkSomeNetSize sz x) = Size.withSingSize sz $ convert proxy x

-- | @since 0.1
instance (FromInteger n, MGroup n, Normed n, Ord n) => Normalize (SomeNetSize d n) where
  type Norm (SomeNetSize d n) = SomeNetSize d n
  normalize (MkSomeNetSize sz x) = Size.withSingSize sz $ normalize x
  {-# INLINE normalize #-}

-- | @since 0.1
instance (Pretty n, SingDirection d) => Pretty (SomeNetSize d n) where
  pretty (MkSomeNetSize sz b) = Size.withSingSize sz $ pretty b
  {-# INLINE pretty #-}

-- | @since 0.1
instance Sized (SomeNetSize d n) where
  type HideSize (SomeNetSize d n) = SomeNetSize d n

  sizeOf (MkSomeNetSize sz _) = Size.ssizeToSize sz
  {-# INLINE sizeOf #-}

  hideSize = id
  {-# INLINE hideSize #-}

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
