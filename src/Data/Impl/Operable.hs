module Data.Impl.Operable where

data Operable f =
  Refer Int String -- With tuple reference and its element
  | Operate (f (Operable f)) -- Operation

-- |Wrapping operation
class Wraps t f where
  wrapInto :: t -> f a

type BasicWraps f = (Wraps Int f, Wraps Float f)
