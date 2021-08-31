-- | Provides typeclasses for comparing types with scalars.
module ByteTypes.Class.ScalarOrd
  ( Scalar,
    ScalarEq (..),
    ScalarOrd (..),
  )
where

import Data.Kind (Type)

-- | Type family for use with 'ScalarEq' and 'ScalarOrd'.
type Scalar :: Type -> Type
type family Scalar a

-- | 'Eq' class for types 'Scalar' comparisons. This is an alternative to
-- the normal 'Eq' class. See 'ScalarOrd' for motivation.
class ScalarEq a where
  (.=) :: a -> Scalar a -> Bool
  (.=) = flip (=.)

  (=.) :: Scalar a -> a -> Bool
  (=.) = flip (.=)

  (./=) :: a -> Scalar a -> Bool
  (./=) x = not . (.=) x

  (/=.) :: Scalar a -> a -> Bool
  (/=.) = flip (./=)

  {-# MINIMAL (.=) | (=.) #-}

infix 4 .=

infix 4 =.

-- | 'Ord' class for types 'Scalar' comparisons. This is an alternative to
-- the normal 'Ord' class. The motivation is as follows:
--
-- Suppose we have to track distances, and the types can either be in
-- kilometers or miles. We make a custom type, and for convenience we
-- implement 'Num' so we can use numeric literals.
--
-- @
-- data Distance a = KiloM a | Miles a
--
-- instance Eq a => Eq (Distance a) where ...
-- instance (Num a, Ord a) => Ord (Distance a) where
--   KiloM x <= KiloM y = x <= y
--   Miles x <= Miles y = x <= y
--   KiloM x <= Miles y = -- convert and compare
--   Miles x <= KiloM y = -- convert and compare
--
-- instance Num a => Num (Distance a) where
--   fromInteger = KiloM . fromInteger
--   ...
--
-- under100 :: Distance Int -> Bool
-- under100 = (<=) 100
--
-- under100 (KiloM 99) -- True
-- under100 (Miles 99) -- False
-- @
--
-- What went wrong with @under100 (Miles 99)@? To implement 'Num' we had to
-- arbitrarily choose a constructor, in this case @KiloM@. But that meant
-- when we did something that looked reasonable, e.g.. @Miles 99 < 100@,
-- this was really @Miles 99 < KiloM 100@, which is false.
--
-- Of course we can manually unwrap our types, but doing so is error-prone
-- and confusing. The 'ScalarOrd' class exists so that we can take advantage
-- of comparing numeric literals to some wrapper type, while also ensuring
-- any type-specific logic in the "usual" comparisons (i.e. 'Ord') does not
-- wreak havoc. In this case, we would have
--
-- @
-- type instance Scalar (Distance a) = a
-- instance ScalarOrd (Distance a) where
--   KiloM x <= k = x <= k
--   Miles x <= k = x <= k
--
-- under100 = (.<= 100)
--
-- under100 (KiloM 99) -- True
-- under100 (Miles 99) -- True
-- @
class ScalarEq a => ScalarOrd a where
  -- LHS
  lcompare :: a -> Scalar a -> Ordering
  lcompare x k
    | x .= k = EQ
    | x .<= k = LT
    | otherwise = GT

  (.<) :: a -> Scalar a -> Bool
  x .< k = lcompare x k == LT

  (.<=) :: a -> Scalar a -> Bool
  x .<= k = case lcompare x k of
    GT -> False
    _ -> True

  (.>) :: a -> Scalar a -> Bool
  x .> k = lcompare x k == GT

  (.>=) :: a -> Scalar a -> Bool
  x .>= k = lcompare x k /= LT

  -- RHS
  rcompare :: Scalar a -> a -> Ordering
  rcompare k x = case lcompare x k of
    LT -> GT
    EQ -> EQ
    GT -> LT

  (<.) :: Scalar a -> a -> Bool
  k <. x = rcompare k x == LT

  (<=.) :: Scalar a -> a -> Bool
  k <=. x = case rcompare k x of
    GT -> False
    _ -> True

  (>.) :: Scalar a -> a -> Bool
  k >. x = rcompare k x == GT

  (>=.) :: Scalar a -> a -> Bool
  k >=. x = rcompare k x /= LT

  {-# MINIMAL ((.<=) | lcompare) #-}
