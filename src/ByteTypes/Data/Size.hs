-- | Provides the 'ByteSize' type and typeclasses for converting
-- between units.
module ByteTypes.Data.Size
  ( ByteSize (..),
    DecByteSize (..),
    IncByteSize (..),
    NextUnit,
    PrevUnit,
  )
where

-- | Byte units.
data ByteSize
  = B
  | KB
  | MB
  | GB
  | TB
  | PB
  deriving (Show)

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

-- | Typeclass for decreasing bytes to the previous units.
class DecByteSize a where
  -- | Prev should involve 'PrevUnit'.
  type Prev a

  prev :: a -> Prev a

-- | Typeclass for increasing bytes to the next units.
class IncByteSize a where
  -- | Next should involve 'NextUnit'.
  type Next a

  next :: a -> Next a
