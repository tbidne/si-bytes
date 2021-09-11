-- | Provides the 'Group' typeclass.
module ByteTypes.Class.Math.Algebra.Group
  ( Group (..),

    -- * NonZero
    NonZero (unNonZero, MkNonZero),
    mkNonZero,
    unsafeNonZero,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)

-- | Defines an algebraic group.
class Eq g => Group g where
  (.+.) :: g -> g -> g
  (.-.) :: g -> g -> g
  g .-. h = g .+. ginv h

  gid :: g

  ginv :: g -> g
  ginv g = gid .-. g

  gabs :: g -> g

  {-# MINIMAL (.+.), ((.-.) | ginv), gid, gabs #-}

infixl 6 .+.

infixl 6 .-.

instance Group Double where
  (.+.) = (+)
  (.-.) = (-)
  gid = 0
  ginv x = - x
  gabs = abs

instance Group Float where
  (.+.) = (+)
  (.-.) = (-)
  gid = 0
  ginv x = - x
  gabs = abs

instance Group Int where
  (.+.) = (+)
  (.-.) = (-)
  gid = 0
  ginv x = - x
  gabs = abs

instance Group Int8 where
  (.+.) = (+)
  (.-.) = (-)
  gid = 0
  ginv x = - x
  gabs = abs

instance Group Int16 where
  (.+.) = (+)
  (.-.) = (-)
  gid = 0
  ginv x = - x
  gabs = abs

instance Group Int32 where
  (.+.) = (+)
  (.-.) = (-)
  gid = 0
  ginv x = - x
  gabs = abs

instance Group Int64 where
  (.+.) = (+)
  (.-.) = (-)
  gid = 0
  ginv x = - x
  gabs = abs

instance Group Integer where
  (.+.) = (+)
  (.-.) = (-)
  gid = 0
  ginv x = - x
  gabs = abs

instance Group Natural where
  (.+.) = (+)
  (.-.) = (-)
  gid = 0
  ginv x = - x
  gabs = abs

instance Group Word where
  (.+.) = (+)
  (.-.) = (-)
  gid = 0
  ginv x = - x
  gabs = abs

instance Group Word8 where
  (.+.) = (+)
  (.-.) = (-)
  gid = 0
  ginv x = - x
  gabs = abs

instance Group Word16 where
  (.+.) = (+)
  (.-.) = (-)
  gid = 0
  ginv x = - x
  gabs = abs

instance Group Word32 where
  (.+.) = (+)
  (.-.) = (-)
  gid = 0
  ginv x = - x
  gabs = abs

instance Group Word64 where
  (.+.) = (+)
  (.-.) = (-)
  gid = 0
  ginv x = - x
  gabs = abs

instance Integral a => Group (Ratio a) where
  (.+.) = (+)
  (.-.) = (-)
  gid = 0
  ginv x = - x
  gabs = abs

-- | Intended to be used for operations requiring non-zero numbers
-- (e.g. division).
newtype NonZero a = MkUnsafeNonZero
  { -- | Unwraps the 'NonZero'.
    unNonZero :: a
  }
  deriving (Eq, Ord, Show)

-- | Allows pattern matching on 'NonZero'.
pattern MkNonZero :: a -> NonZero a
pattern MkNonZero x <- MkUnsafeNonZero x

{-# COMPLETE MkNonZero #-}

-- | Smart constructor for 'NonZero'.
mkNonZero :: Group a => a -> Maybe (NonZero a)
mkNonZero x
  | x /= gid = Just $ MkUnsafeNonZero x
  | otherwise = Nothing

-- | Unsafe constructor for 'NonZero', intended to be used with known
-- constants, e.g., @unsafeNonZero 7@. Exercise restraint!
unsafeNonZero :: Group a => a -> NonZero a
unsafeNonZero x
  | x /= gid = MkUnsafeNonZero x
  | otherwise = error "Passed zero to unsafeNonZero!"
