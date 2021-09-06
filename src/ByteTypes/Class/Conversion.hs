{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Provides typeclasses for convert between byte sizes.
module ByteTypes.Class.Conversion
  ( DecSize (..),
    IncSize (..),
    Conversion (..),
    convert,
    convertWitness,
  )
where

import ByteTypes.Class.Math.Algebra.Field (Field (..))
import ByteTypes.Class.Math.Algebra.Ring (Ring (..))
import ByteTypes.Class.Math.Literal (NumLiteral (..))
import ByteTypes.Data.Size (NextSize, PrevSize, SSize (..), SingSize (..), Size (..))

-- | Typeclass for decrementing bytes to the next units.
class DecSize a where
  prev :: a -> PrevSize a

-- | Typeclass for increasing bytes to the next units.
class IncSize a where
  next :: a -> NextSize a

-- | Provides a common interface for converting between byte sizes.
class Conversion a where
  type Converted (b :: Size) a = r | r -> b

  toB :: a -> Converted 'B a
  toK :: a -> Converted 'K a
  toM :: a -> Converted 'M a
  toG :: a -> Converted 'G a
  toT :: a -> Converted 'T a
  toP :: a -> Converted 'P a

-- | Low level function for converting a numeric literal /from/ the inferred
-- 'SingSize' /to/ the parameter 'Size'. For instance,
-- @
-- convertWitness @SK 'M 5_000 == 5
-- @
--
-- This is slightly more principled than 'convert', but the higher level
-- byte types and functions should still be preferred
-- (e.g. 'ByteTypes.Data.Bytes', 'ByteTypes.Class.Normalize').
convertWitness :: forall s n. (Field n, NumLiteral n, SingSize s) => Size -> n -> n
convertWitness toUnits n = case singSize @s of
  SB -> convert B toUnits n
  SK -> convert K toUnits n
  SM -> convert M toUnits n
  SG -> convert G toUnits n
  ST -> convert T toUnits n
  SP -> convert P toUnits n

-- | Low level function for converting a numeric literal between
-- byte sizes. @convert b1 b2@ converts /from/ @b1@ /to/ @b2@,
-- e.g. @convert G K = \\n -> n * 1_000_000@. The higher level
-- byte types and functions should be preferred
-- (e.g. 'ByteTypes.Data.Bytes', 'ByteTypes.Class.Normalize'), but this is
-- herewhen it is needed.
convert :: (Field n, NumLiteral n) => Size -> Size -> n -> n
convert B B n = n
convert B K n = n .%. fromLit 1_000
convert B M n = n .%. fromLit 1_000_000
convert B G n = n .%. fromLit 1_000_000_000
convert B T n = n .%. fromLit 1_000_000_000_000
convert B P n = n .%. fromLit 1_000_000_000_000_000
convert K B n = n .*. fromLit 1_000
convert K K n = n
convert K M n = n .%. fromLit 1_000
convert K G n = n .%. fromLit 1_000_000
convert K T n = n .%. fromLit 1_000_000_000
convert K P n = n .%. fromLit 1_000_000_000_000
convert M B n = n .*. fromLit 1_000_000
convert M K n = n .*. fromLit 1_000
convert M M n = n
convert M G n = n .%. fromLit 1_000
convert M T n = n .%. fromLit 1_000_000
convert M P n = n .%. fromLit 1_000_000_000
convert G B n = n .*. fromLit 1_000_000_000
convert G K n = n .*. fromLit 1_000_000
convert G M n = n .*. fromLit 1_000
convert G G n = n
convert G T n = n .%. fromLit 1_000
convert G P n = n .%. fromLit 1_000_000
convert T B n = n .*. fromLit 1_000_000_000_000
convert T K n = n .*. fromLit 1_000_000_000
convert T M n = n .*. fromLit 1_000_000
convert T G n = n .*. fromLit 1_000
convert T T n = n
convert T P n = n .%. fromLit 1_000
convert P B n = n .*. fromLit 1_000_000_000_000_000
convert P K n = n .*. fromLit 1_000_000_000_000
convert P M n = n .*. fromLit 1_000_000_000
convert P G n = n .*. fromLit 1_000_000
convert P T n = n .*. fromLit 1_000
convert P P n = n
