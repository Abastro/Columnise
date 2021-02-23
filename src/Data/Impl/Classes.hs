{-# LANGUAGE TypeFamilies #-}
module Data.Impl.Classes (
  Field(..), WithA(..), WithF(..)
) where

-- TODO Proper class for aggregates
-- TODO f for Single f which supports simplification

-- |Class denoting the field type
class (Functor f, Functor (Aggregate f)) => Field f where
  data Aggregate f a
  -- |Impl-only function for retrieving aggregated elements
  elements :: Aggregate f a -> [a]

-- |Containment of kind *
class Field f => WithA p f where
  -- |Wraps into aggregates.
  wrapA :: p a -> Aggregate f a

-- |Containment of kind *->*
class Field f => WithF g f where
  -- |Wraps into the field.
  wrap :: g a -> f a
