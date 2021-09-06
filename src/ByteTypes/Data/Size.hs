-- | Provides the 'Size' type and typeclasses for converting
-- between units.
module ByteTypes.Data.Size
  ( -- * Size Tags
    Size (..),
    SSize (..),
    SingSize (..),
    withSingSize,

    -- * Type Families for Relating Tags
    NextSize,
    PrevSize,
  )
where

import Data.Kind (Type)
import Data.Type.Equality (TestEquality (..), (:~:) (..))

-- | Byte units.
data Size
  = B
  | K
  | M
  | G
  | T
  | P
  deriving (Eq, Ord, Show)

-- | Singleton for 'Size'.
type SSize :: Size -> Type
data SSize s where
  SB :: SSize 'B
  SK :: SSize 'K
  SM :: SSize 'M
  SG :: SSize 'G
  ST :: SSize 'T
  SP :: SSize 'P

instance TestEquality SSize where
  testEquality x y = case (x, y) of
    (SB, SB) -> Just Refl
    (SK, SK) -> Just Refl
    (SM, SM) -> Just Refl
    (SG, SG) -> Just Refl
    (ST, ST) -> Just Refl
    (SP, SP) -> Just Refl
    _ -> Nothing

deriving instance Show (SSize s)

-- | Typeclass for recovering the 'Size' at runtime.
class SingSize s where
  singSize :: SSize s

instance SingSize 'B where singSize = SB

instance SingSize 'K where singSize = SK

instance SingSize 'M where singSize = SM

instance SingSize 'G where singSize = SG

instance SingSize 'T where singSize = ST

instance SingSize 'P where singSize = SP

-- | Singleton \"with\"-style convenience function. Allows us to run a
-- computation @SingSize d => r@ without explicitly pattern-matching
-- every time.
withSingSize :: SSize s -> (SingSize s => r) -> r
withSingSize s x = case s of
  SB -> x
  SK -> x
  SM -> x
  SG -> x
  ST -> x
  SP -> x

-- | Type family that relates units to the next larger one.
type NextSize :: forall k. k -> k
type family NextSize a = r | r -> a

type instance NextSize 'B = 'K

type instance NextSize 'K = 'M

type instance NextSize 'M = 'G

type instance NextSize 'G = 'T

type instance NextSize 'T = 'P

-- | Type family that relates units to the previous smaller one.
type PrevSize :: forall k. k -> k
type family PrevSize a = r | r -> a

type instance PrevSize 'K = 'B

type instance PrevSize 'M = 'K

type instance PrevSize 'G = 'M

type instance PrevSize 'T = 'G

type instance PrevSize 'P = 'T
