-- | Provides the core alternative to 'ByteTypes.Data.Bytes', for when there
-- is a need to distinguish between downloaded and uploaded bytes.
module ByteTypes.Data.Network.NetBytes
  ( -- * Network Bytes
    NetBytes (..),
    unNetBytes,
    netToSSize,
    netToSDirection,

    -- * Unknown Size
    SomeNetSize (..),
    someNetSizeToSDirection,
  )
where

import ByteTypes.Class.Conversion (Conversion (..))
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
import ByteTypes.Data.Bytes (Bytes (..), SomeSize (..))
import ByteTypes.Data.Direction
  ( Direction (..),
    SDirection (..),
    SingDirection (..),
  )
import ByteTypes.Data.Size (SSize (..), SingSize (..), Size (..))
import ByteTypes.Data.Size qualified as Size
import Control.Applicative (liftA2)
import Data.Kind (Type)
import Text.Printf (PrintfArg (..))

-- | Wrapper around the 'Bytes' type that adds the 'Direction' tag.
type NetBytes :: Direction -> Size -> Type -> Type
data NetBytes d s n where
  MkNetBytes :: Bytes s n -> NetBytes d s n

-- | Unwraps the 'NetBytes'.
unNetBytes :: NetBytes d s n -> Bytes s n
unNetBytes (MkNetBytes x) = x

-- | Retrieves the 'SDirection' witness.
netToSDirection :: SingDirection d => NetBytes d s n -> SDirection d
netToSDirection _ = singDirection

-- | Retrieves the 'SingSize' witness.
netToSSize :: SingSize s => NetBytes d s n -> SSize s
netToSSize _ = singSize

deriving instance Show n => Show (NetBytes d s n)

deriving instance Functor (NetBytes d s)

instance Applicative (NetBytes d s) where
  pure = MkNetBytes . pure
  MkNetBytes f <*> MkNetBytes x = MkNetBytes $ f <*> x

instance Monad (NetBytes d s) where
  MkNetBytes x >>= f = MkNetBytes $ x >>= (unNetBytes . f)

instance Eq n => Eq (NetBytes d s n) where
  MkNetBytes x == MkNetBytes y = x == y

instance Ord n => Ord (NetBytes d s n) where
  MkNetBytes x <= MkNetBytes y = x <= y

type instance Scalar (NetBytes d s n) = n

instance Eq n => ScalarEq (NetBytes d s n) where
  MkNetBytes x .= k = x .= k

instance Ord n => ScalarOrd (NetBytes d s n) where
  MkNetBytes x .<= k = x .<= k

instance Ring n => ScalarNum (NetBytes d s n) where
  MkNetBytes x .+ k = MkNetBytes $ x .+ k
  MkNetBytes x .- k = MkNetBytes $ x .- k

instance Group n => Group (NetBytes d s n) where
  (.+.) = liftA2 (.+.)
  (.-.) = liftA2 (.-.)
  gid = MkNetBytes gid
  ginv = fmap ginv
  gabs = fmap gabs

instance Ring n => Module (NetBytes d s n) n where
  MkNetBytes x .* k = MkNetBytes $ x .* k

instance Field n => VectorSpace (NetBytes d s n) n where
  MkNetBytes x .% k = MkNetBytes $ x .% k

instance (Field n, NumLiteral n, SingSize s) => Conversion (NetBytes d s n) where
  type Converted 'B (NetBytes d s n) = NetBytes d 'B n
  type Converted 'K (NetBytes d s n) = NetBytes d 'K n
  type Converted 'M (NetBytes d s n) = NetBytes d 'M n
  type Converted 'G (NetBytes d s n) = NetBytes d 'G n
  type Converted 'T (NetBytes d s n) = NetBytes d 'T n
  type Converted 'P (NetBytes d s n) = NetBytes d 'P n

  toB (MkNetBytes b) = MkNetBytes $ toB b
  toK (MkNetBytes b) = MkNetBytes $ toK b
  toM (MkNetBytes b) = MkNetBytes $ toM b
  toG (MkNetBytes b) = MkNetBytes $ toG b
  toT (MkNetBytes b) = MkNetBytes $ toT b
  toP (MkNetBytes b) = MkNetBytes $ toP b

instance (Field n, NumLiteral n, Ord n, SingSize s) => Normalize (NetBytes d s n) where
  type Norm (NetBytes d s n) = SomeNetSize d n

  normalize (MkNetBytes bytes) = case normalize bytes of
    MkSomeSize sz bytes' -> MkSomeNetSize sz $ MkNetBytes bytes'

instance
  forall d s n.
  (PrintfArg n, SingDirection d, SingSize s) =>
  PrettyPrint (NetBytes d s n)
  where
  pretty (MkNetBytes x) = case singDirection @d of
    SDown -> pretty x <> " Down"
    SUp -> pretty x <> " Up"

-- | Wrapper for 'NetBytes', existentially quantifying the size. This is useful
-- when a function does not know a priori what size it should return, e.g.,
--
-- @
--   getUpTraffic :: IO (SomeNetSize Up Float)
--   getUpTraffic = do
--     (bytes, units) <- getUpTrafficRaw
--     case units of
--       "B" -> MkSomeNetSize SUp $ MkBytes \@B bytes
--       "K" -> MkSomeNetSize SUp $ MkBytes \@K bytes
--       ...
-- @
--
-- 'SomeNetSize' carries along an 'SSize' runtime witness for when we
-- need the size. Its 'Group' functions are 'normalize'd.
--
-- N.B. 'SomeNetSize'\'s instances for lawful typeclasses (e.g. 'Eq', 'Ord',
-- 'Group') are themselves lawful w.r.t. the notion of equivalence defined
-- in its 'Eq' instance.
type SomeNetSize :: Direction -> Type -> Type
data SomeNetSize d n where
  MkSomeNetSize :: SSize s -> NetBytes d s n -> SomeNetSize d n

deriving instance Show n => Show (SomeNetSize d n)

deriving instance Functor (SomeNetSize d)

-- | Note: This instance defines an equivalence relation on 'SomeNetSize' that
-- takes units into account. For instance,
--
-- @
-- MkSomeNetSize SK (MkBytes 1000) == MkSomeNetSize SM (MkBytes 1).
-- @
--
-- Because we expose the underlying @NetBytes@ in several ways (e.g. 'Show',
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
-- let x = MkSomeNetSize SK (MkBytes 1000)
-- let y = MkSomeNetSize SM (MkBytes 1)
-- x == y
-- isK x /= isK y
-- @
--
-- With apologies to Leibniz, such comparisons are too useful to ignore
-- and enable us to implement other lawful classes (e.g. 'Group') that respect
-- this notion of equivalence.
instance (Eq n, Field n, NumLiteral n) => Eq (SomeNetSize d n) where
  x == y = toB x == toB y

instance (Field n, NumLiteral n, Ord n) => Ord (SomeNetSize d n) where
  x <= y = toB x <= toB y

instance (Field n, NumLiteral n, Ord n) => Group (SomeNetSize d n) where
  x .+. y = normalize $ toB x .+. toB y
  x .-. y = normalize $ toB x .-. toB y
  gid = MkSomeNetSize SB gid
  ginv = fmap ginv
  gabs = fmap gabs

instance (Field n, NumLiteral n, Ord n) => Module (SomeNetSize d n) n where
  MkSomeNetSize sz x .* k = MkSomeNetSize sz $ x .* k

instance (Field n, NumLiteral n, Ord n) => VectorSpace (SomeNetSize d n) n where
  MkSomeNetSize sz x .% k = MkSomeNetSize sz $ x .% k

instance (Field n, NumLiteral n) => Conversion (SomeNetSize d n) where
  type Converted 'B (SomeNetSize d n) = NetBytes d 'B n
  type Converted 'K (SomeNetSize d n) = NetBytes d 'K n
  type Converted 'M (SomeNetSize d n) = NetBytes d 'M n
  type Converted 'G (SomeNetSize d n) = NetBytes d 'G n
  type Converted 'T (SomeNetSize d n) = NetBytes d 'T n
  type Converted 'P (SomeNetSize d n) = NetBytes d 'P n

  toB (MkSomeNetSize sz x) = Size.withSingSize sz $ toB x
  toK (MkSomeNetSize sz x) = Size.withSingSize sz $ toK x
  toM (MkSomeNetSize sz x) = Size.withSingSize sz $ toM x
  toG (MkSomeNetSize sz x) = Size.withSingSize sz $ toG x
  toT (MkSomeNetSize sz x) = Size.withSingSize sz $ toT x
  toP (MkSomeNetSize sz x) = Size.withSingSize sz $ toP x

instance (Field n, NumLiteral n, Ord n) => Normalize (SomeNetSize d n) where
  type Norm (SomeNetSize d n) = SomeNetSize d n
  normalize (MkSomeNetSize sz x) = Size.withSingSize sz $ normalize x

instance (PrintfArg n, SingDirection d) => PrettyPrint (SomeNetSize d n) where
  pretty (MkSomeNetSize sz b) = case sz of
    SB -> pretty b
    SK -> pretty b
    SM -> pretty b
    SG -> pretty b
    ST -> pretty b
    SP -> pretty b

-- | Retrieves the 'SingDirection' witness.
someNetSizeToSDirection :: SingDirection d => SomeNetSize d n -> SDirection d
someNetSizeToSDirection _ = singDirection
