module Data.Impl.Operable (
  Operable(..), Wraps(..), BasicWraps
) where

data Operable f =
  Refer Int String -- With tuple reference and its element
  | Operate (f (Operable f)) -- Operation

-- |Wrapping operation
class Wraps t f where
  wrapInto :: t -> f a

-- Placeholder, need to change
type BasicWraps f = (Wraps Int f, Wraps Float f)
