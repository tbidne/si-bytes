-- | Provides the 'Direction' type and singletons.
--
-- @since 0.1
module Data.Bytes.Network.Direction
  ( -- * Direction Tags
    Direction (..),
    SDirection (..),
    SingDirection (..),
    withSingDirection,
    sdirectionToDirection,
  )
where

import Data.Kind (Constraint, Type)
import Data.Type.Equality (TestEquality (..), (:~:) (..))

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
      Show
    )

-- | Singleton for 'Direction'.
--
-- @since 0.1
type SDirection :: Direction -> Type
data SDirection (d :: Direction) where
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
{-# INLINEABLE sdirectionToDirection #-}

-- | @since 0.1
instance TestEquality SDirection where
  testEquality x y = case (x, y) of
    (SDown, SDown) -> Just Refl
    (SUp, SUp) -> Just Refl
    _ -> Nothing
  {-# INLINEABLE testEquality #-}

-- | Typeclass for recovering the 'Direction' at runtime.
--
-- @since 0.1
type SingDirection :: Direction -> Constraint
class SingDirection (d :: Direction) where
  -- | @since 0.1
  singDirection :: SDirection d

-- | @since 0.1
instance SingDirection 'Down where
  singDirection = SDown
  {-# INLINEABLE singDirection #-}

-- | @since 0.1
instance SingDirection 'Up where
  singDirection = SUp
  {-# INLINEABLE singDirection #-}

-- | Singleton \"with\"-style convenience function. Allows us to run a
-- computation @SingDirection d => r@ without explicitly pattern-matching
-- every time.
--
-- @since 0.1
withSingDirection :: SDirection d -> (SingDirection d => r) -> r
withSingDirection s x = case s of
  SDown -> x
  SUp -> x
{-# INLINEABLE withSingDirection #-}
