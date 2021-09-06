-- | Provides the 'ByteSize' type and typeclasses for converting
-- between units.
module ByteTypes.Data.Size
  ( -- * ByteSize Tags
    ByteSize (..),
    SByteSize (..),
    SingByteSize (..),
    withSingByteSize,

    -- * Type Families for Relating Tags
    NextSize,
    PrevSize,
  )
where

import Data.Kind (Type)
import Data.Type.Equality (TestEquality (..), (:~:) (..))

-- | Byte units.
data ByteSize
  = B
  | K
  | M
  | G
  | T
  | P
  deriving (Eq, Ord, Show)

-- | Singleton for 'ByteSize'.
type SByteSize :: ByteSize -> Type
data SByteSize s where
  SB :: SByteSize 'B
  SK :: SByteSize 'K
  SM :: SByteSize 'M
  SG :: SByteSize 'G
  ST :: SByteSize 'T
  SP :: SByteSize 'P

instance TestEquality SByteSize where
  testEquality x y = case (x, y) of
    (SB, SB) -> Just Refl
    (SK, SK) -> Just Refl
    (SM, SM) -> Just Refl
    (SG, SG) -> Just Refl
    (ST, ST) -> Just Refl
    (SP, SP) -> Just Refl
    _ -> Nothing

deriving instance Show (SByteSize s)

-- | Typeclass for recovering the 'ByteSize' at runtime.
class SingByteSize s where
  singByteSize :: SByteSize s

instance SingByteSize 'B where singByteSize = SB

instance SingByteSize 'K where singByteSize = SK

instance SingByteSize 'M where singByteSize = SM

instance SingByteSize 'G where singByteSize = SG

instance SingByteSize 'T where singByteSize = ST

instance SingByteSize 'P where singByteSize = SP

-- | Singleton \"with\"-style convenience function. Allows us to run a
-- computation @SingByteSize d => r@ without explicitly pattern-matching
-- every time.
withSingByteSize :: SByteSize s -> (SingByteSize s => r) -> r
withSingByteSize s x = case s of
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
