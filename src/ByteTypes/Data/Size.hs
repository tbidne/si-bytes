-- | Provides the 'ByteSize' type and typeclasses for converting
-- between units.
module ByteTypes.Data.Size
  ( -- * ByteSize Tags
    ByteSize (..),
    SByteSize (..),
    SingByteSize (..),

    -- * Type Families for Relating Tags
    NextUnit,
    PrevUnit,
  )
where

import Data.Kind (Type)

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
