-- | Provides the 'ByteSize' type and typeclasses for converting
-- between units.
module ByteTypes.Data.Size
  ( -- * ByteSize Tags
    ByteSize (..),
    SByteSize (..),
    SingByteSize (..),
    withSingByteSize,

    -- * Type Families for Relating Tags
    NextUnit,
    PrevUnit,
  )
where

import Data.Kind (Type)
import Data.Type.Equality (TestEquality (..), (:~:) (..))

-- | Byte units.
data ByteSize
  = B
  | KB
  | MB
  | GB
  | TB
  | PB
  deriving (Eq, Ord, Show)

-- | Singleton for 'ByteSize'.
type SByteSize :: ByteSize -> Type
data SByteSize s where
  SB :: SByteSize 'B
  SKB :: SByteSize 'KB
  SMB :: SByteSize 'MB
  SGB :: SByteSize 'GB
  STB :: SByteSize 'TB
  SPB :: SByteSize 'PB

instance TestEquality SByteSize where
  testEquality x y = case (x, y) of
    (SB, SB) -> Just Refl
    (SKB, SKB) -> Just Refl
    (SMB, SMB) -> Just Refl
    (SGB, SGB) -> Just Refl
    (STB, STB) -> Just Refl
    (SPB, SPB) -> Just Refl
    _ -> Nothing

deriving instance Show (SByteSize s)

-- | Typeclass for recovering the 'ByteSize' at runtime.
class SingByteSize s where
  singByteSize :: SByteSize s

instance SingByteSize 'B where singByteSize = SB

instance SingByteSize 'KB where singByteSize = SKB

instance SingByteSize 'MB where singByteSize = SMB

instance SingByteSize 'GB where singByteSize = SGB

instance SingByteSize 'TB where singByteSize = STB

instance SingByteSize 'PB where singByteSize = SPB

-- | Singleton \"with\"-style convenience function. Allows us to run a
-- computation @SingByteSize d => r@ without explicitly pattern-matching
-- every time.
withSingByteSize :: SByteSize s -> (SingByteSize s => r) -> r
withSingByteSize s x = case s of
  SB -> x
  SKB -> x
  SMB -> x
  SGB -> x
  STB -> x
  SPB -> x

-- | Closed type family that relates units to the next larger one.
type NextUnit :: ByteSize -> ByteSize
type family NextUnit a where
  NextUnit 'B = 'KB
  NextUnit 'KB = 'MB
  NextUnit 'MB = 'GB
  NextUnit 'GB = 'TB
  NextUnit 'TB = 'PB
  NextUnit 'PB = 'PB

-- | Closed type family that relates units to the previous smaller one.
type PrevUnit :: ByteSize -> ByteSize
type family PrevUnit a where
  PrevUnit 'B = 'B
  PrevUnit 'KB = 'B
  PrevUnit 'MB = 'KB
  PrevUnit 'GB = 'MB
  PrevUnit 'TB = 'GB
  PrevUnit 'PB = 'TB
