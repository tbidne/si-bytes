-- | Provides the 'NumLiteral' typeclass.
module ByteTypes.Class.Math.Literal
  ( NumLiteral (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)

-- | Replaces 'Num'\'s ' 'fromInteger' functionality for when we do not have
-- a 'Num' instance. Instead of, e.g., @1_000 :: Num a => a@ we have
-- @fromLit 1_000 :: NumLiteral a => a@.
class NumLiteral a where
  fromLit :: Integer -> a

instance NumLiteral Double where
  fromLit = fromInteger

instance NumLiteral Float where
  fromLit = fromInteger

instance NumLiteral Int where
  fromLit = fromInteger

instance NumLiteral Int8 where
  fromLit = fromInteger

instance NumLiteral Int16 where
  fromLit = fromInteger

instance NumLiteral Int32 where
  fromLit = fromInteger

instance NumLiteral Int64 where
  fromLit = fromInteger

instance NumLiteral Integer where
  fromLit = fromInteger

instance NumLiteral Natural where
  fromLit = fromInteger

instance NumLiteral Word where
  fromLit = fromInteger

instance NumLiteral Word8 where
  fromLit = fromInteger

instance NumLiteral Word16 where
  fromLit = fromInteger

instance NumLiteral Word32 where
  fromLit = fromInteger

instance NumLiteral Word64 where
  fromLit = fromInteger

instance Integral a => NumLiteral (Ratio a) where
  fromLit = fromInteger
