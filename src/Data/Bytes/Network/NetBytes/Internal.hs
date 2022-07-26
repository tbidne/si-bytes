{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal module for "Data.Network.NetBytes". The primary
-- difference is this module exposes some underlying details i.e. singleton witnesses. These are hidden by default as they complicate the API.
--
-- @since 0.1
module Data.Bytes.Network.NetBytes.Internal
  ( -- * Network Bytes
    NetBytes (.., MkNetBytesP),
    unNetBytesP,
    netToSSize,
    netToSize,
    netToSDirection,
    netToDirection,
    textToNetBytes,

    -- * Unknown Size
    SomeNetSize (..),
    unSomeNetSize,
    hideNetSize,
    someNetSizeToSize,
    someNetSizeToSDirection,
    someNetSizeToDirection,
    textToSomeNetSize,

    -- ** Helpers
    parseNetBytes,
    parseSomeNetSize,
  )
where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Data.Bytes.Class.Conversion (Conversion (..))
import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Internal (Bytes (..), SomeSize (..))
import Data.Bytes.Internal qualified as BytesI
import Data.Bytes.Network.Direction
  ( Direction (..),
    SDirection (..),
    SingDirection (..),
  )
import Data.Bytes.Network.Direction qualified as Direction
import Data.Bytes.Size (SSize (..), SingSize (..), Size (..))
import Data.Bytes.Size qualified as Size
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
#if !MIN_VERSION_prettyprinter(1, 7, 1)
import Data.Text.Prettyprint.Doc (Pretty (..), (<+>))
#endif
import Data.Void (Void)
import GHC.Generics (Generic)
import GHC.Show qualified as Show
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
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as MP

-- $setup
-- >>> getUpTrafficRaw = pure (40, "K")

-- | Wrapper around the 'Bytes' type that adds the 'Direction' tag.
--
-- ==== __Examples__
-- >>> MkNetBytesP @Up @M 1000
-- MkNetBytesP {unNetBytesP = 1000}
--
-- @since 0.1
type NetBytes :: Direction -> Size -> Type -> Type
newtype NetBytes (d :: Direction) (s :: Size) (n :: Type) = MkNetBytes
  { -- | Unwraps the 'NetBytes'.
    --
    -- @since 0.1
    unNetBytes :: Bytes s n
  }
  deriving stock
    ( -- | @since 0.1
      Generic
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | Convenience function using 'MkNetBytesP'.
--
-- @since 0.1
unNetBytesP :: NetBytes d s n -> n
unNetBytesP (MkNetBytesP x) = x
{-# INLINEABLE unNetBytesP #-}

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
{-# INLINEABLE netToSDirection #-}

-- | Recovers the direction.
--
-- ==== __Examples__
--
-- >>> netToDirection $ MkNetBytesP @Up @M 10
-- Up
--
-- @since 0.1
netToDirection :: SingDirection d => NetBytes d s n -> Direction
netToDirection = Direction.sdirectionToDirection . netToSDirection
{-# INLINEABLE netToDirection #-}

-- | Retrieves the 'SingSize' witness. Can be used to recover the 'Size'.
--
-- >>> netToSSize (MkNetBytesP @Down @K @Int 7)
-- SK
--
-- @since 0.1
netToSSize :: SingSize s => NetBytes d s n -> SSize s
netToSSize _ = singSize
{-# INLINEABLE netToSSize #-}

-- | Recovers the size.
--
-- ==== __Examples__
--
-- >>> netToSize (MkNetBytesP @Up @M 8)
-- M
--
-- @since 0.1
netToSize :: SingSize s => NetBytes d s n -> Size
netToSize = Size.ssizeToSize . netToSSize
{-# INLINEABLE netToSize #-}

-- | @since 0.1
instance (k ~ An_Iso, a ~ m, b ~ n) => LabelOptic "unBytes" k (NetBytes d s m) (NetBytes d s n) a b where
  labelOptic = iso (unBytes . unNetBytes) (MkNetBytes . MkBytes)
  {-# INLINEABLE labelOptic #-}

-- | @since 0.1
instance Show n => Show (NetBytes d s n) where
  showsPrec p (MkNetBytesP x) =
    showParen (p > Show.appPrec) $
      showString "MkNetBytesP {unNetBytesP = "
        . showsPrec Show.appPrec1 x
        . showString "}"
  {-# INLINEABLE showsPrec #-}

-- | @since 0.1
deriving stock instance Functor (NetBytes d s)

-- | @since 0.1
instance Applicative (NetBytes d s) where
  pure = MkNetBytes . pure
  {-# INLINEABLE pure #-}
  MkNetBytes f <*> MkNetBytes x = MkNetBytes $ f <*> x
  {-# INLINEABLE (<*>) #-}

-- | @since 0.1
instance Monad (NetBytes d s) where
  MkNetBytes x >>= f = MkNetBytes $ x >>= (unNetBytes . f)
  {-# INLINEABLE (>>=) #-}

-- | @since 0.1
instance Eq n => Eq (NetBytes d s n) where
  MkNetBytes x == MkNetBytes y = x == y
  {-# INLINEABLE (==) #-}

-- | @since 0.1
instance Ord n => Ord (NetBytes d s n) where
  MkNetBytes x <= MkNetBytes y = x <= y
  {-# INLINEABLE (<=) #-}

-- | @since 0.1
instance ASemigroup n => ASemigroup (NetBytes d s n) where
  (.+.) = liftA2 (.+.)
  {-# INLINEABLE (.+.) #-}

-- | @since 0.1
instance AMonoid n => AMonoid (NetBytes d s n) where
  zero = MkNetBytes zero
  {-# INLINEABLE zero #-}

-- | @since 0.1
instance AGroup n => AGroup (NetBytes d s n) where
  (.-.) = liftA2 (.-.)
  {-# INLINEABLE (.-.) #-}

-- | @since 0.1
instance Normed n => Normed (NetBytes d s n) where
  norm (MkNetBytes b) = MkNetBytes (norm b)
  {-# INLINEABLE norm #-}

-- | @since 0.1
instance MSemigroup n => MSemiSpace (NetBytes d s n) n where
  MkNetBytes x .* k = MkNetBytes $ x .* k
  {-# INLINEABLE (.*) #-}

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
  {-# INLINEABLE toB #-}
  toK (MkNetBytes b) = MkNetBytes $ toK b
  {-# INLINEABLE toK #-}
  toM (MkNetBytes b) = MkNetBytes $ toM b
  {-# INLINEABLE toM #-}
  toG (MkNetBytes b) = MkNetBytes $ toG b
  {-# INLINEABLE toG #-}
  toT (MkNetBytes b) = MkNetBytes $ toT b
  {-# INLINEABLE toT #-}
  toP (MkNetBytes b) = MkNetBytes $ toP b
  {-# INLINEABLE toP #-}
  toE (MkNetBytes b) = MkNetBytes $ toE b
  {-# INLINEABLE toE #-}
  toZ (MkNetBytes b) = MkNetBytes $ toZ b
  {-# INLINEABLE toZ #-}
  toY (MkNetBytes b) = MkNetBytes $ toY b
  {-# INLINEABLE toY #-}

-- | @since 0.1
instance (MGroup n, Normed n, NumLiteral n, Ord n, SingSize s) => Normalize (NetBytes d s n) where
  type Norm (NetBytes d s n) = SomeNetSize d n

  normalize (MkNetBytes bytes) = case normalize bytes of
    MkSomeSize sz bytes' -> MkSomeNetSize sz $ MkNetBytes bytes'
  {-# INLINEABLE normalize #-}

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

-- | Unwraps the 'SomeNetSize'.
--
-- @since 0.1
unSomeNetSize :: SomeNetSize d n -> n
unSomeNetSize (MkSomeNetSize _ b) = unNetBytesP b
{-# INLINEABLE unSomeNetSize #-}

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
  labelOptic = lens unSomeNetSize (\(MkSomeNetSize sz _) x -> MkSomeNetSize sz (MkNetBytesP x))
  {-# INLINEABLE labelOptic #-}

-- | @since 0.1
deriving stock instance Show n => Show (SomeNetSize d n)

-- | @since 0.1
deriving stock instance Functor (SomeNetSize d)

-- | @since 0.1
instance (MGroup n, Eq n, NumLiteral n) => Eq (SomeNetSize d n) where
  x == y = toB x == toB y
  {-# INLINEABLE (==) #-}

-- | @since 0.1
instance (MGroup n, NumLiteral n, Ord n) => Ord (SomeNetSize d n) where
  x <= y = toB x <= toB y
  {-# INLINEABLE (<=) #-}

-- | @since 0.1
instance (ASemigroup n, MGroup n, Normed n, NumLiteral n, Ord n) => ASemigroup (SomeNetSize d n) where
  x .+. y = normalize $ toB x .+. toB y
  {-# INLINEABLE (.+.) #-}

-- | @since 0.1
instance (Normed n, NumLiteral n, Ord n, Semifield n) => AMonoid (SomeNetSize d n) where
  zero = MkSomeNetSize SB zero
  {-# INLINEABLE zero #-}

-- | @since 0.1
instance (Field n, Normed n, NumLiteral n, Ord n) => AGroup (SomeNetSize d n) where
  x .-. y = normalize $ toB x .-. toB y
  {-# INLINEABLE (.-.) #-}

-- | @since 0.1
instance (MGroup n, Normed n, NumLiteral n, Ord n) => MSemiSpace (SomeNetSize d n) n where
  MkSomeNetSize sz x .* k = normalize $ MkSomeNetSize sz $ x .* k
  {-# INLINEABLE (.*) #-}

-- | @since 0.1
instance (MGroup n, Normed n, NumLiteral n, Ord n) => MSpace (SomeNetSize d n) n where
  MkSomeNetSize sz x .% k = normalize $ MkSomeNetSize sz $ x .% k
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
  {-# INLINEABLE toB #-}
  toK (MkSomeNetSize sz x) = Size.withSingSize sz $ toK x
  {-# INLINEABLE toK #-}
  toM (MkSomeNetSize sz x) = Size.withSingSize sz $ toM x
  {-# INLINEABLE toM #-}
  toG (MkSomeNetSize sz x) = Size.withSingSize sz $ toG x
  {-# INLINEABLE toG #-}
  toT (MkSomeNetSize sz x) = Size.withSingSize sz $ toT x
  {-# INLINEABLE toT #-}
  toP (MkSomeNetSize sz x) = Size.withSingSize sz $ toP x
  {-# INLINEABLE toP #-}
  toE (MkSomeNetSize sz x) = Size.withSingSize sz $ toE x
  {-# INLINEABLE toE #-}
  toZ (MkSomeNetSize sz x) = Size.withSingSize sz $ toZ x
  {-# INLINEABLE toZ #-}
  toY (MkSomeNetSize sz x) = Size.withSingSize sz $ toY x
  {-# INLINEABLE toY #-}

-- | @since 0.1
instance (MGroup n, Normed n, NumLiteral n, Ord n) => Normalize (SomeNetSize d n) where
  type Norm (SomeNetSize d n) = SomeNetSize d n
  normalize (MkSomeNetSize sz x) = Size.withSingSize sz $ normalize x
  {-# INLINEABLE normalize #-}

-- | @since 0.1
instance (Pretty n, SingDirection d) => Pretty (SomeNetSize d n) where
  pretty (MkSomeNetSize sz b) = Size.withSingSize sz $ pretty b
  {-# INLINEABLE pretty #-}

-- | Retrieves the 'SingDirection' witness. Can be used to recover the
-- 'Direction'.
--
-- @since 0.1
someNetSizeToSDirection :: SingDirection d => SomeNetSize d n -> SDirection d
someNetSizeToSDirection _ = singDirection
{-# INLINEABLE someNetSizeToSDirection #-}

-- | Recovers the direction.
--
-- ==== __Examples__
--
-- >>> someNetSizeToDirection $ hideNetSize (MkNetBytesP @Up @M 8)
-- Up
--
-- @since 0.1
someNetSizeToDirection :: SingDirection d => SomeNetSize d n -> Direction
someNetSizeToDirection = Direction.sdirectionToDirection . someNetSizeToSDirection
{-# INLINEABLE someNetSizeToDirection #-}

-- | Recovers the size.
--
-- ==== __Examples__
--
-- >>> someNetSizeToSize $ hideNetSize (MkNetBytesP @Up @M 8)
-- M
--
-- @since 0.1
someNetSizeToSize :: SomeNetSize d n -> Size
someNetSizeToSize (MkSomeNetSize sz _) = Size.ssizeToSize sz
{-# INLINEABLE someNetSizeToSize #-}

-- | Attempts to read the text into a 'NetBytes'.
--
-- ==== __Examples__
-- >>> textToNetBytes @Int @Up @B "70"
-- Right (MkNetBytesP {unNetBytesP = 70})
--
-- >>> textToNetBytes @Int @Down @M "cat"
-- Left "1:1:\n  |\n1 | cat\n  | ^\nunexpected 'c'\n"
--
-- @since 0.1
textToNetBytes :: Read n => Text -> Either Text (NetBytes d s n)
textToNetBytes t = case MP.runParser parseNetBytes "" t of
  Left err -> Left . T.pack . MP.errorBundlePretty $ err
  Right netBytes -> Right netBytes
{-# INLINEABLE textToNetBytes #-}

-- | Attempts to read the text into a 'SomeNetSize'. We accept both short and
-- long size e.g. @m@, @mb@, @megabytes@. The text comparisons are
-- case-insensitive, and whitespace between the number and size is optional.
--
-- ==== __Examples__
-- >>> textToSomeNetSize @Int "70 bytes"
-- Right (MkSomeNetSize SB (MkNetBytesP {unNetBytesP = 70}))
--
-- >>> textToSomeNetSize @Int "70 b"
-- Right (MkSomeNetSize SB (MkNetBytesP {unNetBytesP = 70}))
--
-- >>> textToSomeNetSize @Int "70 megabytes"
-- Right (MkSomeNetSize SM (MkNetBytesP {unNetBytesP = 70}))
--
-- >>> textToSomeNetSize @Int "70 gb"
-- Right (MkSomeNetSize SG (MkNetBytesP {unNetBytesP = 70}))
--
-- >>> textToSomeNetSize @Int "70tb"
-- Right (MkSomeNetSize ST (MkNetBytesP {unNetBytesP = 70}))
--
-- >>> textToSomeNetSize @Int "cat"
-- Left "1:1:\n  |\n1 | cat\n  | ^\nunexpected 'c'\n"
--
-- >>> textToSomeNetSize @Int "70 tx"
-- Left "1:5:\n  |\n1 | 70 tx\n  |     ^\nunexpected 'x'\nexpecting \"erabytes\", 'b', end of input, or white space\n"
--
-- @since 0.1
textToSomeNetSize :: Read n => Text -> Either Text (SomeNetSize d n)
textToSomeNetSize t = case MP.runParser parseSomeNetSize "" t of
  Left err -> Left . T.pack . MP.errorBundlePretty $ err
  Right someNetSize -> Right someNetSize
{-# INLINEABLE textToSomeNetSize #-}

parseNetBytes :: Read n => Parsec Void Text (NetBytes d s n)
parseNetBytes = MkNetBytes <$> BytesI.parseBytes
{-# INLINEABLE parseNetBytes #-}

parseSomeNetSize :: Read n => Parsec Void Text (SomeNetSize d n)
parseSomeNetSize = do
  (MkSomeSize sz bytes) <- BytesI.parseSomeSize
  pure $ MkSomeNetSize sz $ MkNetBytes bytes
{-# INLINEABLE parseSomeNetSize #-}
