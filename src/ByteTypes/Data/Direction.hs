-- | Provides the 'Direction' type and singletons.
module ByteTypes.Data.Direction
  ( -- * Direction Tags
    Direction (..),
    SDirection (..),
    SingDirection (..),
    withSingDirection,
  )
where

import Data.Kind (Type)
import Data.Type.Equality (TestEquality (..), (:~:) (..))

-- | Tags for differentiating downloaded vs. uploaded bytes.
data Direction
  = Down
  | Up
  deriving (Eq, Show)

-- | Singleton for 'Direction'.
type SDirection :: Direction -> Type
data SDirection d where
  SDown :: SDirection 'Down
  SUp :: SDirection 'Up

deriving instance Show (SDirection d)

instance TestEquality SDirection where
  testEquality x y = case (x, y) of
    (SDown, SDown) -> Just Refl
    (SUp, SUp) -> Just Refl
    _ -> Nothing

-- | Typeclass for recovering the 'Direction' at runtime.
class SingDirection d where
  singDirection :: SDirection d

instance SingDirection 'Down where singDirection = SDown

instance SingDirection 'Up where singDirection = SUp

-- | Singleton \"with\"-style convenience function. Allows us to run a
-- computation @SingDirection d => r@ without explicitly pattern-matching
-- every time.
withSingDirection :: SDirection d -> (SingDirection d => r) -> r
withSingDirection s x = case s of
  SDown -> x
  SUp -> x
