-- | Provides the 'BytesNum' typeclass.
module ByteTypes.Class.Num
  ( BytesNum (..),
    BytesEq (..),
    BytesOrd (..),
    Scalar,
  )
where

-- | Type family intended to relate a 'Bytes' type to its underlying
-- numeric literal.
type family Scalar n

-- | Replacement for the standard 'Num' class, to be used with
-- our byte types.
class BytesNum a where
  (.+.) :: a -> a -> a
  (.-.) :: a -> a -> a

  (*.) :: Scalar a -> a -> a
  (*.) = flip (.*)

  (.*) :: a -> Scalar a -> a
  (.*) = flip (*.)

  {-# MINIMAL (.+.), (.-.), ((*.) | (.*)) #-}

infixl 6 .+.

infixl 6 .-.

infixl 7 .*

infixl 7 *.

-- | Replacement for the standard 'Eq' class, to be used with our byte
-- types. The 'dot' indicates a bytes type, whereas a lack of one
-- indicates a scalar.
class BytesEq a where
  (.=.), (./=.) :: a -> a -> Bool
  (.=.) x = not . (./=.) x
  (./=.) x = not . (.=.) x

  (=.) :: Scalar a -> a -> Bool
  (=.) = flip (.=)

  (.=) :: a -> Scalar a -> Bool
  (.=) = flip (=.)

  (/=.) :: Scalar a -> a -> Bool
  (/=.) k = not . (=.) k

  (./=) :: a -> Scalar a -> Bool
  (./=) x = not . (.=) x

  {-# MINIMAL ((.=.) | (./=.)), ((=.) | (.=)) #-}

infix 4 .=.

infix 4 ./=.

infix 4 .=

infix 4 =.

-- | Replacement for the standard 'Ord' class, to be used with our byte
-- types. The 'dot' indicates a bytes type, whereas a lack of one
-- indicates a scalar.
class BytesEq a => BytesOrd a where
  (.<=.), (.<.), (.>=.), (.>.) :: a -> a -> Bool
  x .<. y = x .<=. y && not (x .=. y)
  x .>=. y = x .=. y || not (x .<=. y)
  x .>. y = not (x .<=. y)

  (<=.) :: Scalar a -> a -> Bool

  (.<=) :: a -> Scalar a -> Bool
  x .<= k = x .= k || not (k <=. x)

  (<.) :: Scalar a -> a -> Bool
  k <. x = k <=. x && not (k =. x)

  (.<) :: a -> Scalar a -> Bool
  x .< k = x ./= k && not (k <=. x)

  (>=.) :: Scalar a -> a -> Bool
  k >=. x = k =. x || not (k <=. x)

  (.>=) :: a -> Scalar a -> Bool
  x .>= k = x .= k || (k <=. x)

  (>.) :: Scalar a -> a -> Bool
  x >. k = not (x <=. k)

  (.>) :: a -> Scalar a -> Bool
  x .> k = x ./= k && (k <=. x)

  {-# MINIMAL (.<=.), (<=.) #-}

infix 4 .<.

infix 4 .<=.

infix 4 .>=.

infix 4 .>.

infix 4 <.

infix 4 .<

infix 4 <=.

infix 4 .<=

infix 4 >=.

infix 4 .>=

infix 4 >.

infix 4 .>
