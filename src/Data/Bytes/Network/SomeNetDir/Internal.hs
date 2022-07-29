{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal module for "Data.Bytes.Network.SomeNetDir". The primary
-- difference is this module exposes some underlying details i.e. singleton witnesses. These are hidden by default as they complicate the API.
--
-- @since 0.1
module Data.Bytes.Network.SomeNetDir.Internal
  ( -- * Unknown Direction
    SomeNetDir (..),
    hideNetDir,
    someNetDirToSSize,

    -- * Unknown Direction and Size
    SomeNet (..),
    hideNetSizeDir,
  )
where

import Data.Bytes.Class.Conversion (Conversion (..))
import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Class.Parser (Parser (..))
import Data.Bytes.Class.Parser qualified as Parser
import Data.Bytes.Class.Wrapper (Unwrapper (..))
import Data.Bytes.Network.Direction
  ( Directed (..),
    Direction (..),
    SDirection (..),
    SingDirection (..),
  )
import Data.Bytes.Network.Direction qualified as Direction
import Data.Bytes.Network.NetBytes.Internal (NetBytes (..), SomeNetSize (..))
import Data.Bytes.Size (SSize (..), SingSize (..), Size (..), Sized (..))
import Data.Bytes.Size qualified as Size
import Data.Kind (Type)
#if !MIN_VERSION_prettyprinter(1, 7, 1)
import Data.Text.Prettyprint.Doc (Pretty (..))
#endif
import Numeric.Algebra
  ( MGroup,
    MSemiSpace (..),
    MSemigroup (..),
    MSpace (..),
    Normed (..),
  )
import Numeric.Class.Literal (NumLiteral (..))
import Optics.Core (A_Lens, LabelOptic (..), lens)
#if MIN_VERSION_prettyprinter(1, 7, 1)
import Prettyprinter (Pretty (..))
#endif
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- $setup
-- >>> import Data.Bytes.Network.Direction (Direction (..))
-- >>> import Data.Bytes.Network.NetBytes.Internal (NetBytes (..))
-- >>> getMaxTrafficKRaw = pure (40, "Up")
-- >>> getMaxTrafficRaw = pure (40, "Up", "M")

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
--       "down" -> hideNetDir $ MkNetBytesP @Down bytes
--       "up" -> hideNetDir $ MkNetBytesP @Up bytes
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
-- >>> hideNetDir (MkNetBytesP @Up @K 1000) == hideNetDir (MkNetBytesP @Up @K 1000)
-- True
-- >>> hideNetDir (MkNetBytesP @Up @K 1000) /= hideNetDir (MkNetBytesP @Down @K 1000)
-- True
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
someNetDirToSSize :: SingSize s => SomeNetDir s n -> SSize s
someNetDirToSSize _ = singSize
{-# INLINE someNetDirToSSize #-}

-- | Wraps a 'NetBytes' in an existentially quantified 'SomeNetDir'.
--
-- @since 0.1
hideNetDir :: forall d s n. SingDirection d => NetBytes d s n -> SomeNetDir s n
hideNetDir bytes = case singDirection @d of
  SDown -> MkSomeNetDir SDown bytes
  SUp -> MkSomeNetDir SUp bytes
{-# INLINEABLE hideNetDir #-}

-- | @since 0.1
instance (k ~ A_Lens, a ~ m, b ~ n) => LabelOptic "unSomeNetDir" k (SomeNetDir s m) (SomeNetDir s n) a b where
  labelOptic = lens f (\(MkSomeNetDir dx _) x -> MkSomeNetDir dx (MkNetBytesP x))
    where
      f (MkSomeNetDir _ b) = unwrap b
  {-# INLINE labelOptic #-}

-- | @since 0.1
deriving stock instance Show n => Show (SomeNetDir s n)

-- | @since 0.1
deriving stock instance Functor (SomeNetDir s)

-- | @since 0.1
instance (Eq n, NumLiteral n, SingSize s) => Eq (SomeNetDir s n) where
  MkSomeNetDir dx x == MkSomeNetDir dy y =
    case (dx, dy) of
      (SDown, SDown) -> x == y
      (SUp, SUp) -> x == y
      _ -> False
  {-# INLINEABLE (==) #-}

-- | @since 0.1
instance MSemigroup n => MSemiSpace (SomeNetDir s n) n where
  MkSomeNetDir dx x .* k = MkSomeNetDir dx (x .* k)
  {-# INLINE (.*) #-}

-- | @since 0.1
instance MGroup n => MSpace (SomeNetDir s n) n where
  MkSomeNetDir dx x .% k = MkSomeNetDir dx (x .% k)
  {-# INLINEABLE (.%) #-}

-- | @since 0.1
instance Normed n => Normed (SomeNetDir s n) where
  norm (MkSomeNetDir dx x) = MkSomeNetDir dx (norm x)
  {-# INLINE norm #-}

-- | @since 0.1
instance (MGroup n, NumLiteral n, SingSize s) => Conversion (SomeNetDir s n) where
  type Converted B (SomeNetDir s n) = SomeNetDir B n
  type Converted K (SomeNetDir s n) = SomeNetDir K n
  type Converted M (SomeNetDir s n) = SomeNetDir M n
  type Converted G (SomeNetDir s n) = SomeNetDir G n
  type Converted T (SomeNetDir s n) = SomeNetDir T n
  type Converted P (SomeNetDir s n) = SomeNetDir P n
  type Converted E (SomeNetDir s n) = SomeNetDir E n
  type Converted Z (SomeNetDir s n) = SomeNetDir Z n
  type Converted Y (SomeNetDir s n) = SomeNetDir Y n

  toB (MkSomeNetDir dir x) = MkSomeNetDir dir $ toB x
  {-# INLINE toB #-}
  toK (MkSomeNetDir dir x) = MkSomeNetDir dir $ toK x
  {-# INLINE toK #-}
  toM (MkSomeNetDir dir x) = MkSomeNetDir dir $ toM x
  {-# INLINE toM #-}
  toG (MkSomeNetDir dir x) = MkSomeNetDir dir $ toG x
  {-# INLINE toG #-}
  toT (MkSomeNetDir dir x) = MkSomeNetDir dir $ toT x
  {-# INLINE toT #-}
  toP (MkSomeNetDir dir x) = MkSomeNetDir dir $ toP x
  {-# INLINE toP #-}
  toE (MkSomeNetDir dir x) = MkSomeNetDir dir $ toE x
  {-# INLINE toE #-}
  toZ (MkSomeNetDir dir x) = MkSomeNetDir dir $ toZ x
  {-# INLINE toZ #-}
  toY (MkSomeNetDir dir x) = MkSomeNetDir dir $ toY x
  {-# INLINE toY #-}

-- | @since 0.1
instance (MGroup n, Normed n, NumLiteral n, Ord n, SingSize s) => Normalize (SomeNetDir s n) where
  type Norm (SomeNetDir s n) = SomeNet n
  normalize (MkSomeNetDir dir x) =
    case normalize x of
      MkSomeNetSize sz y -> MkSomeNet dir sz y
  {-# INLINEABLE normalize #-}

-- | @since 0.1
instance (Pretty n, SingSize s) => Pretty (SomeNetDir s n) where
  pretty (MkSomeNetDir dir x) =
    Direction.withSingDirection dir $ pretty x
  {-# INLINE pretty #-}

-- | @since 0.1
instance SingSize s => Sized (SomeNetDir s n) where
  sizeOf = Size.ssizeToSize . someNetDirToSSize
  {-# INLINE sizeOf #-}

-- | @since 0.1
instance Directed (SomeNetDir s n) where
  directionOf (MkSomeNetDir d _) = Direction.sdirectionToDirection d
  {-# INLINE directionOf #-}

-- | @since 0.1
instance Unwrapper (SomeNetDir s n) where
  type Unwrapped (SomeNetDir s n) = n
  unwrap (MkSomeNetDir _ b) = unwrap b
  {-# INLINE unwrap #-}

-- | @since 0.1
instance Read n => Parser (SomeNetDir s n) where
  parser = do
    bytes <- Parser.parseDigits
    MPC.space
    dir <- parser
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
--     -- getMaxTrafficRaw :: IO (Double, String, String)
--     (bytes, direction, size) <- getMaxTrafficRaw
--     pure $ case (direction, size) of
--       ("Down", "K") -> hideNetSizeDir $ MkNetBytesP @Down @K bytes
--       ("Up", "M") -> hideNetSizeDir $ MkNetBytesP @Up @M bytes
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
-- >>> hideNetSizeDir (MkNetBytesP @Up @K 1_000) == hideNetSizeDir (MkNetBytesP @Up @M 1)
-- True
--
-- >>> hideNetSizeDir (MkNetBytesP @Up @K 1_000) /= hideNetSizeDir (MkNetBytesP @Down @M 1)
-- True
--
-- @since 0.1
type SomeNet :: Type -> Type
data SomeNet (n :: Type) where
  -- | @since 0.1
  MkSomeNet :: SDirection d -> SSize s -> NetBytes d s n -> SomeNet n

-- | Wraps a 'NetBytes' in an existentially quantified 'SomeNet'.
--
-- @since 0.1
hideNetSizeDir :: forall d s n. (SingDirection d, SingSize s) => NetBytes d s n -> SomeNet n
hideNetSizeDir bytes = case singDirection @d of
  SDown ->
    case singSize @s of
      SB -> MkSomeNet SDown SB bytes
      SK -> MkSomeNet SDown SK bytes
      SM -> MkSomeNet SDown SM bytes
      SG -> MkSomeNet SDown SG bytes
      ST -> MkSomeNet SDown ST bytes
      SP -> MkSomeNet SDown SP bytes
      SE -> MkSomeNet SDown SE bytes
      SZ -> MkSomeNet SDown SZ bytes
      SY -> MkSomeNet SDown SY bytes
  SUp ->
    case singSize @s of
      SB -> MkSomeNet SUp SB bytes
      SK -> MkSomeNet SUp SK bytes
      SM -> MkSomeNet SUp SM bytes
      SG -> MkSomeNet SUp SG bytes
      ST -> MkSomeNet SUp ST bytes
      SP -> MkSomeNet SUp SP bytes
      SE -> MkSomeNet SUp SE bytes
      SZ -> MkSomeNet SUp SZ bytes
      SY -> MkSomeNet SUp SY bytes
{-# INLINEABLE hideNetSizeDir #-}

-- | @since 0.1
instance (k ~ A_Lens, a ~ m, b ~ n) => LabelOptic "unSomeNet" k (SomeNet m) (SomeNet n) a b where
  labelOptic = lens unwrap (\(MkSomeNet dx sz _) x -> MkSomeNet dx sz (MkNetBytesP x))
  {-# INLINE labelOptic #-}

-- | @since 0.1
deriving stock instance Show n => Show (SomeNet n)

-- | @since 0.1
deriving stock instance Functor SomeNet

-- | @since 0.1
instance (MGroup n, Eq n, NumLiteral n) => Eq (SomeNet n) where
  MkSomeNet dx szx x == MkSomeNet dy szy y =
    Size.withSingSize szx $
      Size.withSingSize szy $
        case (dx, dy) of
          (SDown, SDown) -> toB x == toB y
          (SUp, SUp) -> toB x == toB y
          _ -> False
  {-# INLINEABLE (==) #-}

-- | @since 0.1
instance MSemigroup n => MSemiSpace (SomeNet n) n where
  MkSomeNet d s x .* k = MkSomeNet d s (x .* k)
  {-# INLINE (.*) #-}

-- | @since 0.1
instance MGroup n => MSpace (SomeNet n) n where
  MkSomeNet d s x .% k = MkSomeNet d s (x .% k)
  {-# INLINEABLE (.%) #-}

-- | @since 0.1
instance Normed n => Normed (SomeNet n) where
  norm (MkSomeNet d s x) = MkSomeNet d s (norm x)
  {-# INLINE norm #-}

-- | @since 0.1
instance (MGroup n, NumLiteral n) => Conversion (SomeNet n) where
  type Converted B (SomeNet n) = SomeNetDir B n
  type Converted K (SomeNet n) = SomeNetDir K n
  type Converted M (SomeNet n) = SomeNetDir M n
  type Converted G (SomeNet n) = SomeNetDir G n
  type Converted T (SomeNet n) = SomeNetDir T n
  type Converted P (SomeNet n) = SomeNetDir P n
  type Converted E (SomeNet n) = SomeNetDir E n
  type Converted Z (SomeNet n) = SomeNetDir Z n
  type Converted Y (SomeNet n) = SomeNetDir Y n

  toB (MkSomeNet dir sz x) = Size.withSingSize sz $ toB (MkSomeNetDir dir x)
  {-# INLINE toB #-}
  toK (MkSomeNet dir sz x) = Size.withSingSize sz $ toK (MkSomeNetDir dir x)
  {-# INLINE toK #-}
  toM (MkSomeNet dir sz x) = Size.withSingSize sz $ toM (MkSomeNetDir dir x)
  {-# INLINE toM #-}
  toG (MkSomeNet dir sz x) = Size.withSingSize sz $ toG (MkSomeNetDir dir x)
  {-# INLINE toG #-}
  toT (MkSomeNet dir sz x) = Size.withSingSize sz $ toT (MkSomeNetDir dir x)
  {-# INLINE toT #-}
  toP (MkSomeNet dir sz x) = Size.withSingSize sz $ toP (MkSomeNetDir dir x)
  {-# INLINE toP #-}
  toE (MkSomeNet dir sz x) = Size.withSingSize sz $ toE (MkSomeNetDir dir x)
  {-# INLINE toE #-}
  toZ (MkSomeNet dir sz x) = Size.withSingSize sz $ toZ (MkSomeNetDir dir x)
  {-# INLINE toZ #-}
  toY (MkSomeNet dir sz x) = Size.withSingSize sz $ toY (MkSomeNetDir dir x)
  {-# INLINE toY #-}

-- | @since 0.1
instance (MGroup n, Normed n, NumLiteral n, Ord n) => Normalize (SomeNet n) where
  type Norm (SomeNet n) = SomeNet n
  normalize (MkSomeNet dir sz x) =
    case Size.withSingSize sz normalize x of
      MkSomeNetSize sz' x' -> MkSomeNet dir sz' x'
  {-# INLINEABLE normalize #-}

-- | @since 0.1
instance Pretty n => Pretty (SomeNet n) where
  pretty (MkSomeNet dir sz x) =
    Direction.withSingDirection dir $
      Size.withSingSize sz $
        pretty x
  {-# INLINEABLE pretty #-}

-- | @since 0.1
instance Sized (SomeNet n) where
  sizeOf (MkSomeNet _ sz _) = Size.ssizeToSize sz
  {-# INLINE sizeOf #-}

-- | @since 0.1
instance Directed (SomeNet n) where
  directionOf (MkSomeNet d _ _) = Direction.sdirectionToDirection d
  {-# INLINE directionOf #-}

-- | @since 0.1
instance Unwrapper (SomeNet n) where
  type Unwrapped (SomeNet n) = n
  unwrap (MkSomeNet _ _ b) = unwrap b
  {-# INLINE unwrap #-}

-- | @since 0.1
instance Read n => Parser (SomeNet n) where
  parser = do
    bytes <- Parser.parseDigits
    MPC.space
    size <- parser
    MPC.space1
    dir <- parser
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
