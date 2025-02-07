-- | Provides the 'Direction' type and singletons.
--
-- @since 0.1
module Data.Bytes.Network.Direction
  ( -- * Direction Tags
    Direction (..),
    SDirection (..),
    Directed (..),

    -- * Optics
    _Down,
    _Up,
  )
where

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
import Optics.Core (Prism', prism)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

-- | Tags for differentiating downloaded vs. uploaded bytes.
--
-- @since 0.1
type Direction :: Type
data Direction
  = -- | @since 0.1
    Down
  | -- | @since 0.1
    Up
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
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
instance Parser Direction where
  parser =
    MP.choice
      [ parseU Up 'u' "p",
        parseU Down 'd' "own"
      ]
    where
      parseU u ushort ulong = do
        _ <- MPC.char' ushort
        _ <- MP.optional (MPC.string' ulong)
        pure u
  {-# INLINEABLE parser #-}

-- | Singleton for 'Direction'.
--
-- @since 0.1
type SDirection :: Direction -> Type
data SDirection (d :: Direction) where
  -- | @since 0.1
  SDown :: SDirection Down
  -- | @since 0.1
  SUp :: SDirection Up

-- | @since 0.1
deriving stock instance Show (SDirection d)

-- | @since 0.1
instance NFData (SDirection d) where
  rnf SDown = ()
  rnf SUp = ()

-- | @since 0.1
instance TestEquality SDirection where
  testEquality x y = case (x, y) of
    (SDown, SDown) -> Just Refl
    (SUp, SUp) -> Just Refl
    _ -> Nothing
  {-# INLINEABLE testEquality #-}

-- | @since 0.1
type instance Sing = SDirection

-- | @since 0.1
instance SingI Down where
  sing = SDown

-- | @since 0.1
instance SingI Up where
  sing = SUp

-- | @since 0.1
instance SingKind Direction where
  type Demote Direction = Direction

  fromSing SDown = Down
  fromSing SUp = Up

  toSing Down = SomeSing SDown
  toSing Up = SomeSing SUp

-- | Types that have a direction.
--
-- @since 0.1
class Directed a where
  -- | Type used to hide the size.
  --
  -- @since 0.1
  type HideDirection a

  -- | Retrieve the direction.
  --
  -- ==== __Examples__
  --
  -- >>> import Data.Bytes.Network
  -- >>> directionOf (MkNetBytesP @Up @G 7)
  -- Up
  --
  -- >>> directionOf (hideSize $ hideDirection $ MkNetBytesP @Down @M 100)
  -- Down
  --
  -- @since 0.1
  directionOf :: a -> Direction

  -- | Hides the direction.
  --
  -- ==== __Examples__
  --
  -- >>> import Data.Bytes.Network (NetBytes (..), Size (..))
  -- >>> hideDirection (MkNetBytesP @Up @G 7)
  -- MkSomeNetDir SUp (MkNetBytes (MkBytes 7))
  --
  -- @since 0.1
  hideDirection :: a -> HideDirection a

-- | @since 0.1
_Down :: Prism' Direction ()
_Down = prism (const Down) f
  where
    f Down = Right ()
    f x = Left x
{-# INLINE _Down #-}

-- | @since 0.1
_Up :: Prism' Direction ()
_Up = prism (const Up) f
  where
    f Up = Right ()
    f x = Left x
{-# INLINE _Up #-}
