-- | Utilities for property tests.
module Props.Utils
  ( epsEq,
    eqWithEps,
  )
where

-- | Wrapper to be used with 'eqWithEps'.
newtype Epsilon a = MkEpsilon a

-- | Calls 'eqWithEps' with 'Epsilon' of 1.0.
epsEq :: (Fractional a, Ord a) => a -> a -> Bool
epsEq = eqWithEps $ MkEpsilon 1.0

-- | Equals for types in which exact equality is dicey (e.g. 'Double').
eqWithEps :: (Fractional a, Ord a) => Epsilon a -> a -> a -> Bool
eqWithEps (MkEpsilon eps) x y
  | abs (x - y) < eps = True
  | otherwise = False
