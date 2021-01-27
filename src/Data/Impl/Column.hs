{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTSyntax #-}
module Data.Impl.Column (
  Column(..), ColBody(..), defColumn, defWithBody
  , Order(..), PartIndex(..), CombineOp(..), RowPos(..)
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

-- |Denotes ordering of tuple
data Order =
  Asc String | Dsc String
  -- |Omits certain field used for ordering from the visible selection
  | Omit Order
-- |Denotes tuple fields used for partition index
data PartIndex = Keep String | Drop String
data CombineOp = Union | Intersect | Diff
-- |Relative row position
data RowPos = RelPos Int | MinPos | MaxPos

data ColBody f p where
  Known :: String -> ColBody f p
  Lift :: Tuple f -> ColBody f p
  Where :: Single f -> ColBody f p
  -- TODO De-bruijin indexing could make this int obsolete
  Join :: Int -> Column f p -> ColBody f p -> ColBody f p
  Combine :: CombineOp -> Column f p -> Column f p -> ColBody f p

-- |Column data with operation f (Variable is indexed by Int)
data Column f p = Column {
  body :: ColBody f p
  , colOrd :: Maybe [Order]
  , colPart :: Maybe ([PartIndex], [(String, p)])
  , colWindow :: Maybe (RowPos, RowPos)
}

-- |Implementation-only function.
-- Gives default column.
defColumn :: Column f p
defColumn = Column {
  body = Lift $ Tuple []
  , colOrd = Nothing, colPart = Nothing, colWindow = Nothing
}

-- |Implementation-only function.
-- Gives default column with certain body.
defWithBody :: ColBody f p -> Column f p
defWithBody pBody = defColumn { body = pBody }

-- |Monad for column construction.
-- Bind operation joins columns being bound.
newtype MkCol f p a = MkCol (StateT Int (Writer (Endo (ColBody f p))) a)
  deriving (Functor, Applicative, Monad)

-- |Reference to the column being built.
-- Provided for more safety, especially to disallow weaving built columns.
newtype BuildRef f p = BuildRef (Column f p)

type Build f p = MkCol f p (BuildRef f p)

-- |Implementation-only function.
-- Locally finishes builds, processing joins. Does not affect fresh variable state.
finishBuild :: Build f p -> MkCol f p (Column f p)
finishBuild (MkCol build) = MkCol . pass $ do
  (BuildRef fin, Endo apply) <- listen build
  pure (fin { body = apply $ body fin }, const mempty)

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
buildCol build = fst . runWriter . (`evalStateT` 1) $ form where
  MkCol form = finishBuild build

-- |Refers to the known table
known :: String -> Build f p
known = return . BuildRef . defWithBody . Known

-- |Refers to the column being built, binding it for use as a tuple.
refer :: Build f p -> MkCol f p (Tuple f)
refer build = finishBuild build >>= joining

-- |Lifts a tuple to a column.
lift :: Tuple f -> Build f p
lift = return . BuildRef . defWithBody . Lift


-- |Orders the column being built by fields specified by the order.
-- Should be applied at last, as its effect does not leak out when it is inside join.
-- Only outmost one would be in effect when applying order twice.
order :: [Order] -> Build f p -> Build f p
order ords = toBuild . fmap (\x -> x{ colOrd = Just ords }) . finishBuild

-- |Partition
-- Aggregates over entire column when empty list is passed to part index.
-- Should be applied as last, as its effect does not leak out when it is inside join.
-- Only outmost one would be in effect when applying partition twice.
partition :: [PartIndex] -> [(String, p)] -> Build f p -> Build f p
partition index ag = toBuild . fmap (\x -> x{ colPart = Just (index, ag) }) . finishBuild

-- TODO Interaction with union
-- |Window function
-- Works in-place, alike join.
-- Its entire purpose is for aggregates, so it is no-op when not coupled with partition inside.
window :: RowPos -> RowPos -> Build f p -> Build f p
window pri post = toBuild . fmap (\x -> x{ colWindow = Just (pri, post) }) . finishBuild


-- TODO How to handle Union-After-Order: Join?
-- |Union
union :: Build f p -> Build f p -> Build f p
union = fmap toBuild . liftA2 (fmap defWithBody . Combine Union) `on` finishBuild

-- |Intersection
intersection :: Build f p -> Build f p -> Build f p
intersection = fmap toBuild . liftA2 (fmap defWithBody . Combine Intersect) `on` finishBuild

-- |Difference
difference :: Build f p -> Build f p -> Build f p
difference = fmap toBuild . liftA2 (fmap defWithBody . Combine Diff) `on` finishBuild

