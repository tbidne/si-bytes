-- | Provides the 'Normalize' class used for normalizing
-- bytes.
module ByteTypes.Class.Normalize
  ( Normalize (..),
  )
where

-- | Used for normalizing bytes @b@ such that,
--
-- \[
--    1 \le \text{normalize}(b) < 1000 \iff 1\text{ B} \le b < 1000\text{ PB}
-- \]
class Normalize a where
  type Result a
  normalize :: a -> Result a