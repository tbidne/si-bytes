-- | Provides the 'ByteDirection' type and singletons.
module ByteTypes.Data.Direction
  ( -- * ByteDirection Tags
    ByteDirection (..),
    SByteDirection (..),
    SingByteDirection (..),
    withSingByteDirection,
  )
where

import Data.Kind (Type)
import Data.Type.Equality (TestEquality (..), (:~:) (..))

-- | Tags for differentiating downloaded vs. uploaded bytes.
data ByteDirection
  = Down
  | Up
  deriving (Eq, Show)

-- | Singleton for 'ByteDirection'.
type SByteDirection :: ByteDirection -> Type
data SByteDirection d where
  SDown :: SByteDirection 'Down
  SUp :: SByteDirection 'Up

deriving instance Show (SByteDirection d)

instance TestEquality SByteDirection where
  testEquality x y = case (x, y) of
    (SDown, SDown) -> Just Refl
    (SUp, SUp) -> Just Refl
    _ -> Nothing

-- | Typeclass for recovering the 'ByteDirection' at runtime.
class SingByteDirection d where
  singByteDirection :: SByteDirection d

instance SingByteDirection 'Down where singByteDirection = SDown

instance SingByteDirection 'Up where singByteDirection = SUp

-- | Singleton \"with\"-style convenience function. Allows us to run a
-- computation @SingByteDirection d => r@ without explicitly pattern-matching
-- every time.
withSingByteDirection :: SByteDirection d -> (SingByteDirection d => r) -> r
withSingByteDirection s x = case s of
  SDown -> x
  SUp -> x
