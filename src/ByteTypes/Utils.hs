-- | Exports utility functions.
module ByteTypes.Utils
  ( Epsilon (..),
    epsEq,
    eqWithEps,
  )
where

-- | Wrapper to be used with 'eqWithEps'.
newtype Epsilon a = MkEpsilon a

-- | Calls 'eqWithEps' with 'Epsilon' of 0.001.
epsEq :: (Fractional a, Ord a) => a -> a -> Bool
epsEq = eqWithEps $ MkEpsilon 0.001

-- | Equals for types in which exact equality is dicey (e.g. 'Double').
eqWithEps :: (Fractional a, Ord a) => Epsilon a -> a -> a -> Bool
eqWithEps (MkEpsilon eps) x y
  | abs (x - y) < eps = True
  | otherwise = False
