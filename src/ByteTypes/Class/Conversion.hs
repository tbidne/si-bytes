{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides typeclasses for convert between byte sizes.
module ByteTypes.Class.Conversion
  ( DecByteSize (..),
    IncByteSize (..),
    Conversion (..),
    convert,
    convertWitness,
  )
where

import ByteTypes.Class.Math.Algebra.Field (Field (..))
import ByteTypes.Class.Math.Algebra.Ring (Ring (..))
import ByteTypes.Class.Math.Literal (NumLiteral (..))
import ByteTypes.Data.Size (ByteSize (..), SByteSize (..), SingByteSize (..))

-- | Typeclass for decrementing bytes to the next units.
class DecByteSize a where
  -- | Prev should involve 'ByteTypes.Data.Size.PrevUnit'.
  type Prev a = r | r -> a

  prev :: a -> Prev a

-- | Typeclass for increasing bytes to the next units.
class IncByteSize a where
  -- | Next should involve 'ByteTypes.Data.Size.NextUnit'.
  type Next a = r | r -> a

  next :: a -> Next a

-- | Provides a common interface for converting between byte sizes.
class Conversion a where
  type Converted (b :: ByteSize) a = r | r -> b

  toB :: a -> Converted 'B a
  toKB :: a -> Converted 'KB a
  toMB :: a -> Converted 'MB a
  toGB :: a -> Converted 'GB a
  toTB :: a -> Converted 'TB a
  toPB :: a -> Converted 'PB a

-- | Low level function for converting a numeric literal /from/ the inferred
-- 'SingByteSize' /to/ the parameter 'ByteSize'. For instance,
-- @
-- convertWitness @SKB 'MB 5_000 == 5
-- @
--
-- This is slightly more principled than 'convert', but the higher level
-- byte types and functions should still be preferred
-- (e.g. 'ByteTypes.Data.Bytes', 'ByteTypes.Class.Normalize').
convertWitness :: forall s n. (Field n, NumLiteral n, SingByteSize s) => ByteSize -> n -> n
convertWitness toUnits n = case singByteSize @s of
  SB -> convert B toUnits n
  SKB -> convert KB toUnits n
  SMB -> convert MB toUnits n
  SGB -> convert GB toUnits n
  STB -> convert TB toUnits n
  SPB -> convert PB toUnits n

-- | Low level function for converting a numeric literal between
-- byte sizes. @convert b1 b2@ converts /from/ @b1@ /to/ @b2@,
-- e.g. @convert GB KB = \\n -> n * 1_000_000@. The higher level
-- byte types and functions should be preferred
-- (e.g. 'ByteTypes.Data.Bytes', 'ByteTypes.Class.Normalize'), but this is
-- herewhen it is needed.
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
