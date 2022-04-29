-- | Provides the 'Direction' type and singletons.
--
-- @since 0.1
module ByteTypes.Data.Direction
  ( -- * Direction Tags
    Direction (..),
    SDirection (..),
    SingDirection (..),
    withSingDirection,
    sdirectionToDirection,
  )
where

import Data.Kind (Type)
import Data.Type.Equality (TestEquality (..), (:~:) (..))

-- | Tags for differentiating downloaded vs. uploaded bytes.
--
-- @since 0.1
data Direction
  = -- | @since 0.1
    Down
  | -- | @since 0.1
    Up
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | Singleton for 'Direction'.
--
-- @since 0.1
type SDirection :: Direction -> Type
data SDirection d where
  -- | @since 0.1
  SDown :: SDirection 'Down
  -- | @since 0.1
  SUp :: SDirection 'Up

-- | @since 0.1
deriving stock instance Show (SDirection d)

-- | @since 0.1
sdirectionToDirection :: SDirection d -> Direction
sdirectionToDirection SDown = Down
sdirectionToDirection SUp = Up

-- | @since 0.1
instance TestEquality SDirection where
  testEquality x y = case (x, y) of
    (SDown, SDown) -> Just Refl
    (SUp, SUp) -> Just Refl
    _ -> Nothing

-- | Typeclass for recovering the 'Direction' at runtime.
--
-- @since 0.1
class SingDirection d where
  -- | @since 0.1
  singDirection :: SDirection d

-- | @since 0.1
instance SingDirection 'Down where singDirection = SDown

-- | @since 0.1
instance SingDirection 'Up where singDirection = SUp

-- | Singleton \"with\"-style convenience function. Allows us to run a
-- computation @SingDirection d => r@ without explicitly pattern-matching
-- every time.
--
-- @since 0.1
withSingDirection :: SDirection d -> (SingDirection d => r) -> r
withSingDirection s x = case s of
  SDown -> x
  SUp -> x
