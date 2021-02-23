{-# LANGUAGE PatternSynonyms #-}
module Data.Impl.Utils (
  unionWith, unionsWith
  , WithProp(..), pattern RunProp, body, prop, fromProp, fromBody
  , Fresh, runFresh, fresh
) where

import Control.Applicative ( Alternative(..) )
import Control.Monad.State
import Data.Maybe
import Data.List

-- |unionWith for Maybe
unionWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionWith f (Just x) (Just y) = Just $ f x y
unionWith _ s t = s <|> t

-- |unionsWith for Maybe
unionsWith :: (a -> a -> a) -> [Maybe a] -> Maybe a
unionsWith f = fmap (uncurry $ foldl' f) . uncons . catMaybes


-- |Denotes object along with added property
newtype WithProp p a = WithProp{ runProp :: (p, a) }
  deriving (Functor, Applicative) via (,) p
  deriving (Foldable, Traversable)

pattern RunProp :: a -> p -> WithProp p a
pattern RunProp{ body, prop } = WithProp (prop, body)

-- |From property, without body
fromProp :: p -> WithProp p ()
fromProp = WithProp . (, ())

-- |From body, with default property
fromBody :: Monoid p => a -> WithProp p a
fromBody = pure


-- |Monad for fresh variable names.
newtype Fresh a = Fresh (State Int a)
  deriving (Functor, Applicative, Monad) via State Int

-- |Runs fresh monad to get the result.
runFresh :: Fresh a -> a
runFresh (Fresh m) = evalState m 1

-- |Gives fresh variable name.
fresh :: Fresh Int
fresh = Fresh $ get <* modify succ
