{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal module for "Data.Network.NetBytes". The primary
-- difference is this module exposes some underlying details i.e. singleton witnesses. These are hidden by default as they complicate the API.
--
-- @since 0.1
module Data.Bytes.Network.Internal
  ( -- * Network Bytes
    NetBytes (.., MkNetBytesP),
    _MkNetBytes,
    netToSSize,
    netToSDirection,

    -- * Unknown Size
    SomeNetSize (..),
    _MkSomeNetSize,
    someNetSizeToSDirection,

    -- * Unknown Direction
    SomeNetDir (..),
    _MkSomeNetDir,
    someNetDirToSSize,

    -- * Unknown Direction and Size
    SomeNet (..),
    _MkSomeNet,
  )
where

#if !MIN_VERSION_base(4, 18, 0)
import Control.Applicative (liftA2)
#endif
import Control.DeepSeq (NFData (rnf), deepseq)
import Data.Bounds
  ( AnyLowerBounded,
    AnyUpperBounded,
    LowerBounded,
    LowerBoundless,
    UpperBounded,
    UpperBoundless,
  )
import Data.Bytes.Class.Conversion (Conversion (Converted, convert))
import Data.Bytes.Class.Normalize (Normalize (Norm, normalize))
import Data.Bytes.Class.Parser (Parser (parser))
import Data.Bytes.Class.Parser qualified as Parser
import Data.Bytes.Class.RawNumeric (RawNumeric (Raw, toRaw))
import Data.Bytes.Internal (Bytes (MkBytes), SomeSize (MkSomeSize))
import Data.Bytes.Network.Direction
  ( Directed,
    Direction (Down, Up),
    SDirection (SDown, SUp),
    SingDirection (singDirection),
  )
import Data.Bytes.Network.Direction qualified as Direction
import Data.Bytes.Size
  ( SSize (SB, SE, SG, SK, SM, SP, ST, SY, SZ),
    SingSize (singSize),
    Size (B, E, G, K, M, P, T, Y, Z),
    Sized (hideSize),
  )
import Data.Bytes.Size qualified as Size
import Data.Hashable (Hashable (hashWithSalt))
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Numeric.Algebra
  ( AGroup ((.-.)),
    AMonoid (zero),
    ASemigroup ((.+.)),
    Field,
    MGroup,
    MSemiSpace ((.*)),
    MSemigroup,
    MSpace ((.%)),
    Module,
    Normed (norm),
    Ring,
    Semifield,
    Semimodule,
    Semiring,
    SemivectorSpace,
    VectorSpace,
  )
import Numeric.Literal.Integer (FromInteger (afromInteger))
import Numeric.Literal.Rational (FromRational (afromRational))
import Optics.Core (Iso', Review, iso, unto)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- $setup
-- >>> import Data.Bytes.Network.Direction (Direction (..), Directed (..))
-- >>> import Data.Bytes.Size (Size (..), Sized (..))
-- >>> import Data.Bytes.Internal (Bytes (..))
-- >>> getUpTrafficRaw = pure (40, "K")
-- >>> getMaxTrafficKRaw = pure (40, "Up")
-- >>> getMaxTrafficNetRaw = pure (40, "Up", "K")

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
  deriving
    ( -- | @since 0.1
      AnyLowerBounded,
      -- | @since 0.1
      AnyUpperBounded,
      -- | @since 0.1,
      Hashable,
      -- | @since 0.1
      LowerBounded,
      -- | @since 0.1
      LowerBoundless,
      -- | @since 0.1
      UpperBounded,
      -- | @since 0.1
      UpperBoundless
    )
    via n

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
netToSDirection :: (SingDirection d) => NetBytes d s n -> SDirection d
netToSDirection _ = singDirection
{-# INLINE netToSDirection #-}

-- | Retrieves the 'SingSize' witness. Can be used to recover the 'Size'.
--
-- >>> netToSSize (MkNetBytesP @Down @K @Int 7)
-- SK
--
-- @since 0.1
netToSSize :: (SingSize s) => NetBytes d s n -> SSize s
netToSSize _ = singSize
{-# INLINE netToSSize #-}

-- | 'Iso'' between 'NetBytes' and underlying 'Bytes'.
--
-- ==== __Examples__
--
-- >>> import Optics.Core (review, view)
-- >>> review _MkNetBytes (MkBytes @K @Int 70)
-- MkNetBytes (MkBytes 70)
--
-- >>> view _MkNetBytes (MkNetBytes $ MkBytes @K @Int 70)
-- MkBytes 70
--
-- @since 0.1
_MkNetBytes :: forall s d n. Iso' (NetBytes d s n) (Bytes s n)
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
instance (FromInteger n) => FromInteger (NetBytes d s n) where
  afromInteger = MkNetBytes . afromInteger
  {-# INLINE afromInteger #-}

-- | @since 0.1
instance (FromRational n) => FromRational (NetBytes d s n) where
  afromRational = MkNetBytes . afromRational
  {-# INLINE afromRational #-}

-- | @since 0.1
instance (ASemigroup n) => ASemigroup (NetBytes d s n) where
  (.+.) = liftA2 (.+.)
  {-# INLINE (.+.) #-}

-- | @since 0.1
instance (AMonoid n) => AMonoid (NetBytes d s n) where
  zero = MkNetBytes zero
  {-# INLINE zero #-}

-- | @since 0.1
instance (AGroup n) => AGroup (NetBytes d s n) where
  (.-.) = liftA2 (.-.)
  {-# INLINE (.-.) #-}

-- | @since 0.1
instance (Normed n) => Normed (NetBytes d s n) where
  norm (MkNetBytes b) = MkNetBytes (norm b)
  {-# INLINE norm #-}

-- | @since 0.1
instance (MSemigroup n) => MSemiSpace (NetBytes d s n) n where
  MkNetBytes x .* k = MkNetBytes $ x .* k
  {-# INLINE (.*) #-}

-- | @since 0.1
instance (MGroup n) => MSpace (NetBytes d s n) n where
  MkNetBytes x .% k = MkNetBytes $ x .% k
  {-# INLINEABLE (.%) #-}

-- | @since 0.1
instance (Semiring n) => Semimodule (NetBytes d s n) n

-- | @since 0.1
instance (Ring n) => Module (NetBytes d s n) n

-- | @since 0.1
instance (Semifield n) => SemivectorSpace (NetBytes d s n) n

-- | @since 0.1
instance (Field n) => VectorSpace (NetBytes d s n) n

-- | @since 0.1
instance (FromInteger n, MGroup n, SingSize s) => Conversion (NetBytes d s n) where
  type Converted t (NetBytes d s n) = NetBytes d t n

  convert :: forall t. (SingSize t) => Proxy t -> NetBytes d s n -> NetBytes d t n
  convert proxy (MkNetBytes x) = MkNetBytes $ convert proxy x

-- | @since 0.1
instance (FromInteger n, MGroup n, Normed n, Ord n, SingSize s) => Normalize (NetBytes d s n) where
  type Norm (NetBytes d s n) = SomeNetSize d n

  normalize (MkNetBytes bytes) = case normalize bytes of
    MkSomeSize sz bytes' -> MkSomeNetSize sz $ MkNetBytes bytes'
  {-# INLINE normalize #-}

-- | @since 0.1
instance (SingSize s) => Sized (NetBytes d s n) where
  type HideSize (NetBytes d s n) = SomeNetSize d n

  sizeOf = Size.ssizeToSize . netToSSize
  {-# INLINE sizeOf #-}

  hideSize b@(MkNetBytes _) = MkSomeNetSize (singSize @s) b
  {-# INLINE hideSize #-}

-- | @since 0.1
instance (SingDirection d) => Directed (NetBytes d s n) where
  type HideDirection (NetBytes d s n) = SomeNetDir s n

  directionOf = Direction.sdirectionToDirection . netToSDirection
  {-# INLINE directionOf #-}

  hideDirection b@(MkNetBytes _) = MkSomeNetDir (singDirection @d) b
  {-# INLINE hideDirection #-}

-- | @since 0.1
instance RawNumeric (NetBytes d s n) where
  type Raw (NetBytes d s n) = n
  toRaw (MkNetBytes b) = toRaw b
  {-# INLINE toRaw #-}

-- | @since 0.1
instance (Read n) => Parser (NetBytes d s n) where
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

-- | 'Iso'' between 'SomeNetSize' and underlying 'NetBytes'. Performs any
-- necessary conversions when going from @SomeNetSize d n -> NetBytes d s n@.
--
-- ==== __Examples__
--
-- >>> import Optics.Core (review, view)
-- >>> review _MkSomeNetSize (MkNetBytesP @Up @K @Int 70)
-- MkSomeNetSize SK (MkNetBytes (MkBytes 70))
--
-- >>> (view _MkSomeNetSize (hideSize $ MkNetBytesP @Up @K @Int 70)) :: NetBytes Up B Int
-- MkNetBytes (MkBytes 70000)
--
-- @since 0.1
_MkSomeNetSize :: forall s d n. (FromInteger n, MGroup n, SingSize s) => Iso' (SomeNetSize d n) (NetBytes d s n)
_MkSomeNetSize = iso (convert Proxy) hideSize
{-# INLINE _MkSomeNetSize #-}

-- | @since 0.1
deriving stock instance (Show n) => Show (SomeNetSize d n)

-- | @since 0.1
instance (FromInteger n, Hashable n, MGroup n) => Hashable (SomeNetSize d n) where
  hashWithSalt i (MkSomeNetSize sz x) =
    i `hashWithSalt` Size.ssizeToSize sz `hashWithSalt` x

-- | @since 0.1
instance (NFData n) => NFData (SomeNetSize d n) where
  rnf (MkSomeNetSize sz x) = sz `deepseq` x `deepseq` ()

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
instance (FromInteger n) => FromInteger (SomeNetSize d n) where
  afromInteger = MkSomeNetSize SB . afromInteger
  {-# INLINE afromInteger #-}

-- | Fixed size 'B'.
--
-- @since 0.1
instance (FromRational n) => FromRational (SomeNetSize d n) where
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
instance (MGroup n) => MSemiSpace (SomeNetSize d n) n where
  MkSomeNetSize sz x .* k = MkSomeNetSize sz $ x .* k
  {-# INLINE (.*) #-}

-- | @since 0.1
instance (MGroup n) => MSpace (SomeNetSize d n) n where
  MkSomeNetSize sz x .% k = MkSomeNetSize sz $ x .% k
  {-# INLINEABLE (.%) #-}

-- | @since 0.1
instance (FromInteger n, Semifield n) => Semimodule (SomeNetSize d n) n

-- | @since 0.1
instance (Field n, FromInteger n) => Module (SomeNetSize d n) n

-- | @since 0.1
instance (FromInteger n, Semifield n) => SemivectorSpace (SomeNetSize d n) n

-- | @since 0.1
instance (FromInteger n, Field n) => VectorSpace (SomeNetSize d n) n

-- | @since 0.1
instance (FromInteger n, MGroup n) => Conversion (SomeNetSize d n) where
  type Converted t (SomeNetSize d n) = NetBytes d t n

  convert :: forall t. (SingSize t) => Proxy t -> SomeNetSize d n -> NetBytes d t n
  convert proxy (MkSomeNetSize sz x) = Size.withSingSize sz $ convert proxy x

-- | @since 0.1
instance (FromInteger n, MGroup n, Normed n, Ord n) => Normalize (SomeNetSize d n) where
  type Norm (SomeNetSize d n) = SomeNetSize d n
  normalize (MkSomeNetSize sz x) = Size.withSingSize sz $ normalize x
  {-# INLINE normalize #-}

-- | @since 0.1
instance Sized (SomeNetSize d n) where
  type HideSize (SomeNetSize d n) = SomeNetSize d n

  sizeOf (MkSomeNetSize sz _) = Size.ssizeToSize sz
  {-# INLINE sizeOf #-}

  hideSize = id
  {-# INLINE hideSize #-}

-- | @since 0.1
instance (SingDirection d) => Directed (SomeNetSize d n) where
  type HideDirection (SomeNetSize d n) = SomeNet n

  directionOf = Direction.sdirectionToDirection . someNetSizeToSDirection
  {-# INLINE directionOf #-}

  hideDirection (MkSomeNetSize sz b) = MkSomeNet (singDirection @d) sz b
  {-# INLINE hideDirection #-}

-- | @since 0.1
instance RawNumeric (SomeNetSize d n) where
  type Raw (SomeNetSize d n) = n
  toRaw (MkSomeNetSize _ b) = toRaw b
  {-# INLINE toRaw #-}

-- | @since 0.1
instance (Read n) => Parser (SomeNetSize d n) where
  parser = do
    MkSomeSize sz bytes <- parser
    pure $ MkSomeNetSize sz (MkNetBytes bytes)

-- | Retrieves the 'SingDirection' witness. Can be used to recover the
-- 'Direction'.
--
-- @since 0.1
someNetSizeToSDirection :: (SingDirection d) => SomeNetSize d n -> SDirection d
someNetSizeToSDirection _ = singDirection
{-# INLINE someNetSizeToSDirection #-}

unNetBytes :: NetBytes d s n -> Bytes s n
unNetBytes (MkNetBytes x) = x

-- | Wrapper for 'NetBytes', existentially quantifying the direction.
-- This is useful when a function does not know a priori what
-- direction it should return e.g.
--
-- >>> :{
--   getMaxTraffic :: IO (SomeNetDir K Double)
--   getMaxTraffic = do
--     -- getMaxTrafficKRaw :: IO (Double, String)
--     (bytes, direction) <- getMaxTrafficKRaw
--     pure $ case direction of
--       "down" -> hideDirection $ MkNetBytesP @Down bytes
--       "up" -> hideDirection $ MkNetBytesP @Up bytes
--       _ -> error "bad direction"
-- :}
--
-- We deliberately do not provide instances for SomeX classes that could be
-- used to combine arbitrary 'SomeNetDir's (e.g. 'Applicative',
-- 'Numeric.Algebra.Additive.AGroup.AGroup'), as that would defeat the purpose
-- of enforcing the distinction between upload and downloaded bytes.
--
-- Equality is determined by the usual equivalence class -- that takes units
-- into account -- and by considering the direction.
--
-- >>> let x = MkNetBytesP @Up @K 1000 :: NetBytes Up K Int
-- >>> let y = MkNetBytesP @Down @K 1000 :: NetBytes Down K Int
-- >>> hideDirection x == hideDirection x
-- True
-- >>> hideDirection x == hideDirection y
-- False
--
-- Notice no 'Ord' instance is provided, as we provide no ordering for
-- 'Data.Byte.Network.Direction'.
--
-- @since 0.1
type SomeNetDir :: Size -> Type -> Type
data SomeNetDir (s :: Size) (n :: Type) where
  -- | @since 0.1
  MkSomeNetDir :: SDirection d -> NetBytes d s n -> SomeNetDir s n

-- | Retrieves the 'SingSize' witness. Can be used to recover the
-- 'Size'.
--
-- @since 0.1
someNetDirToSSize :: (SingSize s) => SomeNetDir s n -> SSize s
someNetDirToSSize _ = singSize
{-# INLINE someNetDirToSSize #-}

-- | 'Review' between 'SomeNetDir' and underlying 'NetBytes'. This is not an
-- iso (i.e. only allows @NetBytes -> SomeNetDir@) because the opposite
-- direction would require dropping the 'Direction' and the user arbitrarily
-- choosing a new one.
--
-- ==== __Examples__
--
-- >>> import Optics.Core (review)
-- >>> review _MkSomeNetDir (MkNetBytesP @Up @K @Int 70)
-- MkSomeNetDir SUp (MkNetBytes (MkBytes 70))
--
-- @since 0.1
_MkSomeNetDir :: forall s d n. (SingDirection d) => Review (SomeNetDir s n) (NetBytes d s n)
_MkSomeNetDir = unto (\b -> MkSomeNetDir (netToSDirection b) b)
{-# INLINE _MkSomeNetDir #-}

-- | @since 0.1
deriving stock instance (Show n) => Show (SomeNetDir s n)

-- | @since 0.1
instance
  (FromInteger n, Hashable n, MGroup n, SingSize s) =>
  Hashable (SomeNetDir s n)
  where
  hashWithSalt i (MkSomeNetDir d x) =
    i `hashWithSalt` Direction.sdirectionToDirection d `hashWithSalt` x

-- | @since 0.1
instance (NFData n) => NFData (SomeNetDir s n) where
  rnf (MkSomeNetDir d x) = d `deepseq` x `deepseq` ()

-- | @since 0.1
deriving stock instance Functor (SomeNetDir s)

-- | @since 0.1
instance (Eq n, FromInteger n, SingSize s) => Eq (SomeNetDir s n) where
  MkSomeNetDir dx x == MkSomeNetDir dy y =
    case (dx, dy) of
      (SDown, SDown) -> x == y
      (SUp, SUp) -> x == y
      _ -> False
  {-# INLINEABLE (==) #-}

-- | @since 0.1
instance (MSemigroup n) => MSemiSpace (SomeNetDir s n) n where
  MkSomeNetDir dx x .* k = MkSomeNetDir dx (x .* k)
  {-# INLINE (.*) #-}

-- | @since 0.1
instance (MGroup n) => MSpace (SomeNetDir s n) n where
  MkSomeNetDir dx x .% k = MkSomeNetDir dx (x .% k)
  {-# INLINEABLE (.%) #-}

-- | @since 0.1
instance (Normed n) => Normed (SomeNetDir s n) where
  norm (MkSomeNetDir dx x) = MkSomeNetDir dx (norm x)
  {-# INLINE norm #-}

-- | @since 0.1
instance (FromInteger n, MGroup n, SingSize s) => Conversion (SomeNetDir s n) where
  type Converted t (SomeNetDir s n) = SomeNetDir t n

  convert :: forall t. (SingSize t) => Proxy t -> SomeNetDir s n -> SomeNetDir t n
  convert proxy (MkSomeNetDir dir x) = MkSomeNetDir dir $ convert proxy x

-- | @since 0.1
instance (FromInteger n, MGroup n, Normed n, Ord n, SingSize s) => Normalize (SomeNetDir s n) where
  type Norm (SomeNetDir s n) = SomeNet n
  normalize (MkSomeNetDir dir x) =
    case normalize x of
      MkSomeNetSize sz y -> MkSomeNet dir sz y
  {-# INLINEABLE normalize #-}

-- | @since 0.1
instance (SingSize s) => Sized (SomeNetDir s n) where
  type HideSize (SomeNetDir s n) = SomeNet n
  sizeOf = Size.ssizeToSize . someNetDirToSSize
  {-# INLINE sizeOf #-}

  hideSize (MkSomeNetDir d b) = MkSomeNet d (singSize @s) b
  {-# INLINE hideSize #-}

-- | @since 0.1
instance Directed (SomeNetDir s n) where
  type HideDirection (SomeNetDir s n) = SomeNetDir s n

  directionOf (MkSomeNetDir d _) = Direction.sdirectionToDirection d
  {-# INLINE directionOf #-}

  hideDirection = id
  {-# INLINE hideDirection #-}

-- | @since 0.1
instance RawNumeric (SomeNetDir s n) where
  type Raw (SomeNetDir s n) = n
  toRaw (MkSomeNetDir _ b) = toRaw b
  {-# INLINE toRaw #-}

-- | @since 0.1
instance (Read n) => Parser (SomeNetDir s n) where
  parser = do
    bytes <- Parser.parseDigits
    MPC.space
    dir <- parser
    MPC.space
    MP.eof
    pure $ case dir of
      Down -> MkSomeNetDir SDown $ MkNetBytesP bytes
      Up -> MkSomeNetDir SUp $ MkNetBytesP bytes
  {-# INLINEABLE parser #-}

-- | Wrapper for 'NetBytes', existentially quantifying the size /and/
-- direction. This is useful when a function does not know a priori what
-- size or direction it should return e.g.
--
-- >>> :{
--   getMaxTraffic :: IO (SomeNet Double)
--   getMaxTraffic = do
--     -- getMaxTrafficNetRaw :: IO (Double, String, String)
--     (bytes, direction, size) <- getMaxTrafficNetRaw
--     pure $ case (direction, size) of
--       ("Down", "K") -> hideDirection $ hideSize $ MkNetBytesP @Down @K bytes
--       ("Up", "M") -> hideDirection $ hideSize $ MkNetBytesP @Up @M bytes
--       _ -> error "todo"
-- :}
--
-- 'SomeNet' carries along 'SDirection' and 'SSize' runtime witnesses
-- for recovering the 'Data.Bytes.Network.Direction'
-- and 'Size', respectively.
--
-- This instance uses the same equivalence relation from 'SomeNetSize'
-- w.r.t the size, and also includes an equality check on the direction.
-- Thus we have, for instance,
--
-- >>> let x = MkNetBytesP 1_000 :: NetBytes Up K Int
-- >>> let y = MkNetBytesP 1 :: NetBytes Up M Int
-- >>> let z = MkNetBytesP 1_000 :: NetBytes Down K Int
-- >>> hideDirection (hideSize x) == hideDirection (hideSize y)
-- True
--
-- >>> hideDirection (hideSize x) == hideDirection (hideSize z)
-- False
--
-- @since 0.1
type SomeNet :: Type -> Type
data SomeNet (n :: Type) where
  -- | @since 0.1
  MkSomeNet :: SDirection d -> SSize s -> NetBytes d s n -> SomeNet n

-- | 'Review' between 'SomeNet' and underlying 'NetBytes'. This is not an
-- iso (i.e. only allows @NetBytes -> SomeNet@) because the opposite
-- direction would require dropping the 'Direction' and the user arbitrarily
-- choosing a new one.
--
-- ==== __Examples__
--
-- >>> import Optics.Core (review)
-- >>> review _MkSomeNet (MkNetBytesP @Up @K @Int 70)
-- MkSomeNet SUp SK (MkNetBytes (MkBytes 70))
--
-- @since 0.1
_MkSomeNet :: forall s d n. (SingDirection d, SingSize s) => Review (SomeNet n) (NetBytes d s n)
_MkSomeNet = unto (\b -> MkSomeNet (netToSDirection b) (netToSSize b) b)
{-# INLINE _MkSomeNet #-}

-- | @since 0.1
deriving stock instance (Show n) => Show (SomeNet n)

-- | @since 0.1
instance (FromInteger n, Hashable n, MGroup n) => Hashable (SomeNet n) where
  hashWithSalt i (MkSomeNet d s x) =
    i
      `hashWithSalt` Direction.sdirectionToDirection d
      `hashWithSalt` Size.ssizeToSize s
      `hashWithSalt` x

-- | @since 0.1
instance (NFData n) => NFData (SomeNet n) where
  rnf (MkSomeNet d s x) = d `deepseq` s `deepseq` x `deepseq` ()

-- | @since 0.1
deriving stock instance Functor SomeNet

-- | @since 0.1
instance (Eq n, FromInteger n, MGroup n) => Eq (SomeNet n) where
  MkSomeNet dx szx x == MkSomeNet dy szy y =
    Size.withSingSize szx $
      Size.withSingSize szy $
        case (dx, dy) of
          (SDown, SDown) -> convert @_ @B Proxy x == convert Proxy y
          (SUp, SUp) -> convert @_ @B Proxy x == convert Proxy y
          _ -> False
  {-# INLINEABLE (==) #-}

-- | @since 0.1
instance (MSemigroup n) => MSemiSpace (SomeNet n) n where
  MkSomeNet d s x .* k = MkSomeNet d s (x .* k)
  {-# INLINE (.*) #-}

-- | @since 0.1
instance (MGroup n) => MSpace (SomeNet n) n where
  MkSomeNet d s x .% k = MkSomeNet d s (x .% k)
  {-# INLINEABLE (.%) #-}

-- | @since 0.1
instance (Normed n) => Normed (SomeNet n) where
  norm (MkSomeNet d s x) = MkSomeNet d s (norm x)
  {-# INLINE norm #-}

-- | @since 0.1
instance (FromInteger n, MGroup n) => Conversion (SomeNet n) where
  type Converted t (SomeNet n) = SomeNetDir t n

  convert :: forall t. (SingSize t) => Proxy t -> SomeNet n -> SomeNetDir t n
  convert proxy (MkSomeNet dir sz x) = Size.withSingSize sz $ MkSomeNetDir dir $ convert proxy x

-- | @since 0.1
instance (FromInteger n, MGroup n, Normed n, Ord n) => Normalize (SomeNet n) where
  type Norm (SomeNet n) = SomeNet n
  normalize (MkSomeNet dir sz x) =
    case Size.withSingSize sz normalize x of
      MkSomeNetSize sz' x' -> MkSomeNet dir sz' x'
  {-# INLINEABLE normalize #-}

-- | @since 0.1
instance Sized (SomeNet n) where
  type HideSize (SomeNet n) = SomeNet n
  sizeOf (MkSomeNet _ sz _) = Size.ssizeToSize sz
  {-# INLINE sizeOf #-}

  hideSize = id
  {-# INLINE hideSize #-}

-- | @since 0.1
instance Directed (SomeNet n) where
  type HideDirection (SomeNet n) = SomeNet n

  directionOf (MkSomeNet d _ _) = Direction.sdirectionToDirection d
  {-# INLINE directionOf #-}

  hideDirection = id
  {-# INLINE hideDirection #-}

-- | @since 0.1
instance RawNumeric (SomeNet n) where
  type Raw (SomeNet n) = n
  toRaw (MkSomeNet _ _ b) = toRaw b
  {-# INLINE toRaw #-}

-- | @since 0.1
instance (Read n) => Parser (SomeNet n) where
  parser = do
    bytes <- Parser.parseDigits
    MPC.space
    size <- parser
    MPC.space1
    dir <- parser
    MPC.space
    MP.eof
    pure $ case dir of
      Down -> case size of
        B -> MkSomeNet SDown SB $ MkNetBytesP bytes
        K -> MkSomeNet SDown SK $ MkNetBytesP bytes
        M -> MkSomeNet SDown SM $ MkNetBytesP bytes
        G -> MkSomeNet SDown SG $ MkNetBytesP bytes
        T -> MkSomeNet SDown ST $ MkNetBytesP bytes
        P -> MkSomeNet SDown SP $ MkNetBytesP bytes
        E -> MkSomeNet SDown SE $ MkNetBytesP bytes
        Z -> MkSomeNet SDown SZ $ MkNetBytesP bytes
        Y -> MkSomeNet SDown SY $ MkNetBytesP bytes
      Up -> case size of
        B -> MkSomeNet SUp SB $ MkNetBytesP bytes
        K -> MkSomeNet SUp SK $ MkNetBytesP bytes
        M -> MkSomeNet SUp SM $ MkNetBytesP bytes
        G -> MkSomeNet SUp SG $ MkNetBytesP bytes
        T -> MkSomeNet SUp ST $ MkNetBytesP bytes
        P -> MkSomeNet SUp SP $ MkNetBytesP bytes
        E -> MkSomeNet SUp SE $ MkNetBytesP bytes
        Z -> MkSomeNet SUp SZ $ MkNetBytesP bytes
        Y -> MkSomeNet SUp SY $ MkNetBytesP bytes
  {-# INLINEABLE parser #-}
