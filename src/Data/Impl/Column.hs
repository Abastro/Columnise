{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTSyntax #-}
module Data.Impl.Column (
  Column(..), Order(..), CombineOp(..), RowPos(..)
  , MkCol, BuildRef, Build
  , buildCol, finishBuild, joining, known, refer, lift
  , order, union, intersection, difference, partition, window
) where

import Control.Applicative ( Applicative(..) )
import Control.Monad.Writer ( Writer, runWriter, tell, listen, pass )
import Control.Monad.State ( StateT(..), evalStateT, get, modify )
import Data.Function ( on )
import Data.Monoid ( Endo(..) )

import Data.RefTuple ( Tuple(..), Single(..) )

-- TODO Typecheck column statements
-- TODO Insert, Update, Delete

data Order =
  Asc String | Dsc String
  -- |Omits certain field used for ordering from the visible selection
  | Omit Order
data CombineOp = Union | Intersect | Diff
data RowPos = RelPos Int | MinPos | MaxPos

-- |Column data with operation f (Variable is indexed by Int)
data Column f p where
  Known :: String -> Column f p
  Lift :: Tuple f -> Column f p
  Where :: Single f -> Column f p
  Join :: Int -> Column f p -> Column f p -> Column f p

  Combine :: CombineOp -> Column f p -> Column f p -> Column f p
  Order :: [Order] -> Column f p -> Column f p
  Partition :: Tuple f -> [(String, p)] -> Column f p -> Column f p
  Window :: RowPos -> RowPos -> Column f p -> Column f p

-- |Monad for column construction.
-- Bind operation joins columns being bound.
newtype MkCol f p a = MkCol (StateT Int (Writer (Endo (Column f p))) a)
  deriving (Functor, Applicative, Monad)

-- |Reference to the column being built.
-- Provided for more safety, especially to disallow weaving columns
newtype BuildRef f p = BuildRef (Column f p)

type Build f p = MkCol f p (BuildRef f p)

-- |Implementation-only function.
-- Locally finishes builds, processing joins. Does not affect fresh variable state.
finishBuild :: Build f p -> MkCol f p (Column f p)
finishBuild (MkCol build) = MkCol . pass $ do
  (BuildRef fin, Endo apply) <- listen build
  pure (apply fin, const mempty)

-- |Implementation-only function.
-- Puts column back into build.
toBuild :: MkCol f p (Column f p) -> Build f p
toBuild = fmap BuildRef

-- |Implementation-only function.
-- Joins (& binds) given column.
joining :: Column f p -> MkCol f p (Tuple f)
joining col = do
  index <- fresh
  MkCol . tell $ Endo $ Join index col
  return $ TRef index
  where
    fresh = MkCol $ get <* modify succ


-- |Builds the column
buildCol :: Build f p -> Column f p
buildCol (MkCol form) = apply fin where
  (BuildRef fin, Endo apply) = runWriter . (`evalStateT` 1) $ form

-- |Refers to the known table
known :: String -> Build f p
known = pure . BuildRef . Known

-- |Refers to the column being built, binding it for use as a tuple.
refer :: Build f p -> MkCol f p (Tuple f)
refer build = finishBuild build >>= joining

-- |Lifts a tuple to a column.
lift :: Tuple f -> Build f p
lift s = return . BuildRef $ Lift s


-- |Gives ordered column. Only the last order statement gets in effect.
order :: [Order] -> Build f p -> Build f p
order ords = toBuild . fmap (Order ords) . finishBuild

-- TODO How to handle Union-After-Order: Join?
-- |Union
union :: Build f p -> Build f p -> Build f p
union = fmap toBuild . liftA2 (Combine Union) `on` finishBuild

-- |Intersection
intersection :: Build f p -> Build f p -> Build f p
intersection = fmap toBuild . liftA2 (Combine Intersect) `on` finishBuild

-- |Difference
difference :: Build f p -> Build f p -> Build f p
difference = fmap toBuild . liftA2 (Combine Diff) `on` finishBuild


-- TODO Partition perhaps should use other stuffs
-- |Partition
partition :: Tuple f -> [(String, p)] -> Build f p -> Build f p
partition index ag = toBuild . fmap (Partition index ag) . finishBuild

-- |Window function
window :: RowPos -> RowPos -> Build f p -> Build f p
window pri post = toBuild . fmap (Window pri post) . finishBuild
