-- | Provides the 'Group' typeclass.
module ByteTypes.Class.Math.Algebra.Group
  ( Group (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)

-- | Defines an algebraic group.
class Group g where
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
