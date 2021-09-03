-- | Provides the 'ScalarNum' class.
module ByteTypes.Class.Math.Scalar.Num
  ( ScalarNum (..),
  )
where

import ByteTypes.Class.Math.Scalar.Scalar (Scalar)

-- | Defines an asymmetric group-like structure in terms of 'Scalar'.
-- This is intended as a convenience for types that do not implement 'Num'
-- for whatever reason, but we still want to provide addition/subtraction
-- with numeric literals.
class ScalarNum m where
  (.+) :: m -> Scalar m -> m
  (.+) = flip (+.)

  (.-) :: m -> Scalar m -> m

  (+.) :: Scalar m -> m -> m
  (+.) = flip (.+)

  {-# MINIMAL ((.+) | (+.)), (.-) #-}

infixl 6 .+

infixl 6 +.

infixl 6 .-
