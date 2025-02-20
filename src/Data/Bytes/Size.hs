{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Size' type and typeclasses for converting
-- between units.
--
-- @since 0.1
module Data.Bytes.Size
  ( -- * Size Tags
    Size (..),
    SSize (..),

    -- * Sized Types
    Sized (..),

    -- * Type Families for Relating Tags
    NextSize,
    PrevSize,

    -- * Optics
    _B,
    _K,
    _M,
    _G,
    _T,
    _P,
    _E,
    _Z,
    _Y,
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData (rnf))
import Data.Bytes.Class.Parser (Parser (parser))
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Singletons as X
  ( Sing,
    SingI (sing),
    SingKind (Demote, fromSing, toSing),
    SomeSing (SomeSing),
  )
import Data.Type.Equality (TestEquality (testEquality), (:~:) (Refl))
import GHC.Generics (Generic)
import GHC.TypeLits (ErrorMessage (Text), TypeError)
import Optics.Core (Prism', prism)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- | Byte units.
--
-- @since 0.1
type Size :: Type
data Size
  = -- | Bytes
    --
    -- @since 0.1
    B
  | -- | Kilobytes
    --
    -- @since 0.1
    K
  | -- | Megabytes
    --
    -- @since 0.1
    M
  | -- | Gigabytes
    --
    -- @since 0.1
    G
  | -- | Terabytes
    --
    -- @since 0.1
    T
  | -- | Petabytes
    --
    -- @since 0.1
    P
  | -- | Exabytes
    --
    -- @since 0.1
    E
  | -- | Zettabytes
    --
    -- @since 0.1
    Z
  | -- | Yottabytes
    --
    -- @since 0.1
    Y
  deriving stock
    ( -- | @since 0.1
      Bounded,
      -- | @since 0.1
      Enum,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      Hashable,
      -- | @since 0.1
      NFData
    )

-- | @since 0.1
_B :: Prism' Size ()
_B = prism (const B) f
  where
    f B = Right ()
    f x = Left x
{-# INLINE _B #-}

-- | @since 0.1
_K :: Prism' Size ()
_K = prism (const K) f
  where
    f K = Right ()
    f x = Left x
{-# INLINE _K #-}

-- | @since 0.1
_M :: Prism' Size ()
_M = prism (const M) f
  where
    f M = Right ()
    f x = Left x
{-# INLINE _M #-}

-- | @since 0.1
_G :: Prism' Size ()
_G = prism (const G) f
  where
    f G = Right ()
    f x = Left x
{-# INLINE _G #-}

-- | @since 0.1
_T :: Prism' Size ()
_T = prism (const T) f
  where
    f T = Right ()
    f x = Left x
{-# INLINE _T #-}

-- | @since 0.1
_P :: Prism' Size ()
_P = prism (const P) f
  where
    f P = Right ()
    f x = Left x
{-# INLINE _P #-}

-- | @since 0.1
_E :: Prism' Size ()
_E = prism (const E) f
  where
    f E = Right ()
    f x = Left x
{-# INLINE _E #-}

-- | @since 0.1
_Z :: Prism' Size ()
_Z = prism (const Z) f
  where
    f Z = Right ()
    f x = Left x
{-# INLINE _Z #-}

-- | @since 0.1
_Y :: Prism' Size ()
_Y = prism (const Y) f
  where
    f Y = Right ()
    f x = Left x
{-# INLINE _Y #-}

-- | @since 0.1
instance Parser Size where
  parser =
    MP.choice
      [ parseB,
        parseU K 'k' "ilobytes",
        parseU M 'm' "egabytes",
        parseU G 'g' "igabytes",
        parseU T 't' "erabytes",
        parseU P 'p' "etabytes",
        parseU E 'e' "xabytes",
        parseU Z 'z' "ettabytes",
        parseU Y 'y' "ottabytes"
      ]
    where
      parseB = do
        _ <- MPC.char' 'b'
        _ <- MP.optional (MPC.string' "ytes")
        pure B
      parseU u ushort ulong = do
        _ <- MPC.char' ushort
        _ <- MP.optional (MP.try (MPC.string' "b") <|> MPC.string' ulong)
        pure u
  {-# INLINEABLE parser #-}

-- | Singleton for 'Size'.
--
-- @since 0.1
type SSize :: Size -> Type
data SSize (s :: Size) where
  -- | @since 0.1
  SB :: SSize B
  -- | @since 0.1
  SK :: SSize K
  -- | @since 0.1
  SM :: SSize M
  -- | @since 0.1
  SG :: SSize G
  -- | @since 0.1
  ST :: SSize T
  -- | @since 0.1
  SP :: SSize P
  -- | @since 0.1
  SE :: SSize E
  -- | @since 0.1
  SZ :: SSize Z
  -- | @since 0.1
  SY :: SSize Y

-- | @since 0.1
instance NFData (SSize s) where
  rnf SB = ()
  rnf SK = ()
  rnf SM = ()
  rnf SG = ()
  rnf ST = ()
  rnf SP = ()
  rnf SE = ()
  rnf SZ = ()
  rnf SY = ()

-- | @since 0.1
instance TestEquality SSize where
  testEquality x y = case (x, y) of
    (SB, SB) -> Just Refl
    (SK, SK) -> Just Refl
    (SM, SM) -> Just Refl
    (SG, SG) -> Just Refl
    (ST, ST) -> Just Refl
    (SP, SP) -> Just Refl
    (SE, SE) -> Just Refl
    (SZ, SZ) -> Just Refl
    (SY, SY) -> Just Refl
    _ -> Nothing
  {-# INLINEABLE testEquality #-}

-- | @since 0.1
deriving stock instance Show (SSize s)

-- | @since 0.1
type instance Sing = SSize

-- | @since 0.1
instance SingI B where
  sing = SB

-- | @since 0.1
instance SingI K where
  sing = SK

-- | @since 0.1
instance SingI M where
  sing = SM

-- | @since 0.1
instance SingI G where
  sing = SG

-- | @since 0.1
instance SingI T where
  sing = ST

-- | @since 0.1
instance SingI P where
  sing = SP

-- | @since 0.1
instance SingI E where
  sing = SE

-- | @since 0.1
instance SingI Z where
  sing = SZ

-- | @since 0.1
instance SingI Y where
  sing = SY

-- | @since 0.1
instance SingKind Size where
  type Demote Size = Size

  fromSing SB = B
  fromSing SK = K
  fromSing SM = M
  fromSing SG = G
  fromSing ST = T
  fromSing SP = P
  fromSing SE = E
  fromSing SZ = Z
  fromSing SY = Y

  toSing B = SomeSing SB
  toSing K = SomeSing SK
  toSing M = SomeSing SM
  toSing G = SomeSing SG
  toSing T = SomeSing ST
  toSing P = SomeSing SP
  toSing E = SomeSing SE
  toSing Z = SomeSing SZ
  toSing Y = SomeSing SY

-- | Type family that relates units to the next larger one.
--
-- ==== __Examples__
--
-- >>> :kind! NextSize 'M
-- NextSize 'M :: Size
-- = G
--
-- >>> :kind! NextSize 'Y
-- NextSize 'Y :: Size
-- = (TypeError ...)
--
-- @since 0.1
type NextSize :: Size -> Size
type family NextSize (s :: Size) = (t :: Size) where
  NextSize B = K
  NextSize K = M
  NextSize M = G
  NextSize G = T
  NextSize T = P
  NextSize P = E
  NextSize E = Z
  NextSize Z = Y
  NextSize Y = TypeError ('Text "The byte unit Y does not have a 'next size'.")

-- | Type family that relates units to the previous smaller one.
--
-- ==== __Examples__
--
-- >>> :kind! PrevSize 'M
-- PrevSize 'M :: Size
-- = K
--
-- >>> :kind! PrevSize 'B
-- PrevSize 'B :: Size
-- = (TypeError ...)
--
-- @since 0.1
type PrevSize :: Size -> Size
type family PrevSize (s :: Size) = (t :: Size) where
  PrevSize B = TypeError ('Text "The byte unit B does not have a 'previous size'.")
  PrevSize K = B
  PrevSize M = K
  PrevSize G = M
  PrevSize T = G
  PrevSize P = T
  PrevSize E = P
  PrevSize Z = E
  PrevSize Y = Z

-- | Types that have a size.
--
-- @since 0.1
class Sized a where
  -- | Type used to hide the size.
  --
  -- @since 0.1
  type HideSize a

  -- | Retrieves the size.
  --
  -- ==== __Examples__
  --
  -- >>> import Data.Bytes (Bytes (..))
  -- >>> sizeOf (MkBytes @G 7)
  -- G
  --
  -- >>> sizeOf (hideSize $ MkBytes @M 7)
  -- M
  --
  -- >>> import Data.Bytes.Network (NetBytes (..), Direction (..))
  -- >>> sizeOf (hideSize $ MkNetBytesP @Up @M 7)
  -- M
  --
  -- @since 0.1
  sizeOf :: a -> Size

  -- | Hides the size.
  --
  -- ==== __Examples__
  --
  -- >>> import Data.Bytes (Bytes (..))
  -- >>> hideSize (MkBytes @G 7)
  -- MkSomeSize SG (MkBytes 7)

  -- >>> import Data.Bytes.Network (NetBytes (..), Direction (..))
  -- >>> hideSize (MkNetBytesP @Down @K 400)
  --
  -- @since 0.1
  hideSize :: a -> HideSize a
