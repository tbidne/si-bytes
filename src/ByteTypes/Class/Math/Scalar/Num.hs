-- | Provides the 'ScalarNum' class.
module ByteTypes.Class.Math.Scalar.Num
  ( ScalarNum (..),
  )
where

import ByteTypes.Class.Math.Algebra (Group (..), Module)
import ByteTypes.Class.Math.Scalar.Scalar (Scalar)

-- | Defines an asymmetric group-like structure in terms of 'Scalar'.
-- This is intended as a convenience for types that do not implement 'Num'
-- for whatever reason, but we still want to provide addition/subtraction
-- with numeric literals.
class (Module m (Scalar m)) => ScalarNum m where
  (.+) :: m -> Scalar m -> m
  (.+) = flip (+.)

  (.-) :: m -> Scalar m -> m
  x .- k = x .+ ginv k

  (+.) :: Scalar m -> m -> m
  (+.) = flip (.+)

  (-.) :: Scalar m -> m -> m
  k -. x = ginv x .+ k

  {-# MINIMAL ((.+) | (+.)) #-}

infixl 6 .+

infixl 6 +.

infixl 6 .-

infixl 6 -.
