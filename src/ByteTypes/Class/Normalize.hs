-- | Provides the 'Normalize' typeclass.
module ByteTypes.Class.Normalize
  ( Normalize (..),
  )
where

-- | Used for normalizing bytes @b@ such that
--
-- \[
--    1 \le \text{normalize}(b) < 1000 \iff 1\text{ B} \le b < 1000\text{ PB}.
-- \]
--
-- If we view @normalize@ as single function taking in a type and a value,
-- then the only law is idempotence:
--
-- \[
--   \textrm{normalize} \circ \textrm{normalize} = \textrm{normalize}.
-- \]
--
-- Restricting @normalize@ to a single type (i.e. one instance) also yields
-- injectivity:
--
-- \[
--   \textrm{normalize}(x) = \textrm{normalize}(y) \implies x = y.
-- \]
--
-- N.B. Notice that \(\textrm{normalize} : \mathcal{C} \rightarrow \mathcal{C}\) does __not__ define a homomorpism.
-- That is, the law
--
-- \[
--   \textrm{normalize}(f(x, y)) = f(\textrm{normalize}(x), \textrm{normalize}(y))
-- \]
--
-- cannot possibly hold. For one, it is possible the result of @f@ will not be
-- normalized regardless of its arguments. Even if we had such @f@, being
-- homomorphic would require following the /field/ distributive law,
--
-- \[ k (x + y) = k x + k y, \]
--
-- which most 'Num' instances violate. For instance, even 'Integer' violates
-- this when accounting for division, as the action induced by division is
-- not free. In other words, it violates
--
-- \[
--   \forall n \in \mathbb{Z},\;\; d_1, d_2, \in \mathbb{Z}^\times,\quad \frac{n}{d_1} = \frac{n}{d_2} \implies d_1 = d_2.
-- \]
--
-- The moral of this story is that normalization should typically be the
-- /last/ operation performed, most likely used for display purposes.
-- There is usually no need to normalize multiple times.
class Normalize a where
  type Norm a
  normalize :: a -> Norm a
