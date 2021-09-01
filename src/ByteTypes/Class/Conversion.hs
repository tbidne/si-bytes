-- | Provides typeclasses for converting between byte sizes.
module ByteTypes.Class.Conversion
  ( DecByteSize (..),
    IncByteSize (..),
    Conversion (..),
    convert,
  )
where

import ByteTypes.Class.Math (Field (..), NumLiteral (..), Ring (..))
import ByteTypes.Data.Size (ByteSize (..))

-- | Typeclass for decreasing bytes to the previous units.
class DecByteSize a where
  -- | Prev should involve 'ByteTypes.Data.Size.PrevUnit'.
  type Prev a

  prev :: a -> Prev a

-- | Typeclass for increasing bytes to the next units.
class IncByteSize a where
  -- | Next should involve 'ByteTypes.Data.Size.NextUnit'.
  type Next a

  next :: a -> Next a

-- | Provides a common interface for converting between byte sizes.
class Conversion a where
  type Converted (b :: ByteSize) a
  toB :: a -> Converted 'B a
  toKB :: a -> Converted 'KB a
  toMB :: a -> Converted 'MB a
  toGB :: a -> Converted 'GB a
  toTB :: a -> Converted 'TB a
  toPB :: a -> Converted 'PB a

-- | Low level function for converting a numeric literal between
-- byte sizes. @convert b1 b2@ converts /from/ @b1@ /to/ @b2@,
-- e.g. @convert GB KB = \\n -> n * 1_000_000@. The higher level
-- byte types and functions should be preferred
-- (e.g. 'ByteTypes.Data.Bytes', 'Normalize'), but this is here
-- when it is needed.
convert :: (Field n, NumLiteral n) => ByteSize -> ByteSize -> n -> n
convert B B n = n
convert B KB n = n .%. fromLit 1_000
convert B MB n = n .%. fromLit 1_000_000
convert B GB n = n .%. fromLit 1_000_000_000
convert B TB n = n .%. fromLit 1_000_000_000_000
convert B PB n = n .%. fromLit 1_000_000_000_000_000
convert KB B n = n .*. fromLit 1_000
convert KB KB n = n
convert KB MB n = n .%. fromLit 1_000
convert KB GB n = n .%. fromLit 1_000_000
convert KB TB n = n .%. fromLit 1_000_000_000
convert KB PB n = n .%. fromLit 1_000_000_000_000
convert MB B n = n .*. fromLit 1_000_000
convert MB KB n = n .*. fromLit 1_000
convert MB MB n = n
convert MB GB n = n .%. fromLit 1_000
convert MB TB n = n .%. fromLit 1_000_000
convert MB PB n = n .%. fromLit 1_000_000_000
convert GB B n = n .*. fromLit 1_000_000_000
convert GB KB n = n .*. fromLit 1_000_000
convert GB MB n = n .*. fromLit 1_000
convert GB GB n = n
convert GB TB n = n .%. fromLit 1_000
convert GB PB n = n .%. fromLit 1_000_000
convert TB B n = n .*. fromLit 1_000_000_000_000
convert TB KB n = n .*. fromLit 1_000_000_000
convert TB MB n = n .*. fromLit 1_000_000
convert TB GB n = n .*. fromLit 1_000
convert TB TB n = n
convert TB PB n = n .%. fromLit 1_000
convert PB B n = n .*. fromLit 1_000_000_000_000_000
convert PB KB n = n .*. fromLit 1_000_000_000_000
convert PB MB n = n .*. fromLit 1_000_000_000
convert PB GB n = n .*. fromLit 1_000_000
convert PB TB n = n .*. fromLit 1_000
convert PB PB n = n
