module Data.Impl.Classes (
  With(..), With1(..)
) where

-- TODO Class for aggregates

-- |Containment of kind *
class With p q where
  -- |Wraps form p to united form q. Use with caution, this can cause type mismatch.
  wrap :: p -> q

-- |Containment of kind *->*
class With1 f g where
  -- |Wraps form f to unified form g. Use with caution, this can cause type mismatch.
  wrap1 :: f a -> g a
