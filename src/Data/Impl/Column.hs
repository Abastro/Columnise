{-# LANGUAGE GADTSyntax #-}
module Data.Impl.Column (
  Column, ColProps(..), ColBody(..), defColumn, defWithBody
  , Omits(..), omit, OrdDir(..), Order, asc, dsc, PartIndex, part
  , CombineOp(..), RowPos(..)
  , Fresh, fresh, runFresh, MkCol, Build, BuiltColumn
  , build, joining, known, refer, lift
  , order, union, intersection, difference, partitions, window
) where

import Control.Applicative
import Control.Monad.Writer hiding ( lift )
import Control.Monad.State hiding ( lift )
import qualified Control.Monad.Trans as T
import Data.Function ( on )

import Data.RefTuple ( Tuple(..), Single(..) )

-- TODO Insert, Update, Delete
-- TODO Typecheck column statements

-- |Denotes omittable
data Omits a = Omits { omits :: Bool, ins :: a } deriving Eq
-- |Omits certain field used for ordering/partition from the selection output.
-- Should be coupled with select or tuple construction.
-- If it is directly from referring a column, it is a no-op.
omit :: Omits a -> Omits a
omit x = x{ omits = True }

-- |Ordering directions
data OrdDir = Asc | Dsc deriving Eq
-- |Denotes ordering of tuple
type Order = Omits (OrdDir, String)
-- |Ascending order
asc :: String -> Order
asc = Omits False . (Asc, )
-- |Descending order
dsc :: String -> Order
dsc = Omits False . (Dsc, )

-- |Denotes tuple fields used for partition index
type PartIndex = Omits String
-- |Creates partition index
part :: String -> PartIndex
part = Omits False

data CombineOp = Union | Intersect | Diff deriving Eq
-- |Relative row position
data RowPos = MinPos | RelPos Int | MaxPos deriving (Eq, Ord)

data ColBody f p where
  Known :: String -> ColBody f p
  Lift :: Tuple f -> ColBody f p
  Where :: Single f -> ColBody f p
  -- MAYBE De-bruijin indexing could make this int obsolete
  Join :: Int -> Column f p -> ColBody f p -> ColBody f p
  Combine :: CombineOp -> ColBody f p -> ColBody f p -> ColBody f p

data ColProps p a = CProps {
  colBody :: a
  , colOrd :: [Order]
  , colPart :: Maybe ([PartIndex], [(String, p)])
  , colWindow :: Maybe (RowPos, RowPos)
} deriving (Functor, Foldable, Traversable)

-- |Column data with operation f (Variable is indexed by Int)
type Column f p = ColProps p (ColBody f p)

-- |Implementation-only function.
-- Gives default column.
defColumn :: Column f p
defColumn = CProps {
  colBody = Lift mempty, colOrd = [], colPart = Nothing, colWindow = Nothing
}

-- |Implementation-only function.
-- Gives default column with certain body.
defWithBody :: ColBody f p -> Column f p
defWithBody pBody = pBody <$ defColumn


-- |Monad for fresh variable names.
newtype Fresh a = Fresh (State Int a)
  deriving (Functor, Applicative, Monad)

-- |Implementation-only function.
-- Runs fresh monad to get the result.
runFresh :: Fresh a -> a
runFresh (Fresh m) = evalState m 1

-- |Implementation-only function.
-- Gives fresh variable name.
fresh :: Fresh Int
fresh = Fresh $ get <* modify succ

-- |Monad for column construction.
-- Bind operation joins columns being bound.
newtype MkCol f p a = MkCol ((WriterT (Endo (ColBody f p)) Fresh) a)
  deriving (Functor, Applicative, Monad)

type Build f p = MkCol f p (Column f p)
type BuiltColumn f p = Fresh (Column f p)

-- |Implementation-only function.
-- Locally finishes builds, processing joins. Does not affect fresh binding state.
finishBuild :: Build f p -> Build f p
finishBuild (MkCol build) = MkCol . pass $ do
  (fin, Endo apply) <- listen build
  pure (apply <$> fin, const mempty)

-- |Implementation-only function.
-- Joins (& binds) given column.
joining :: Column f p -> MkCol f p (Tuple f)
joining col = do
  index <- MkCol $ T.lift fresh
  MkCol . tell $ Endo $ Join index col
  return $ TRef index

-- |Builds the column
build :: Build f p -> BuiltColumn f p
build build = fmap fst . runWriterT $ form where
  MkCol form = finishBuild build

-- |Refers to the known table
known :: String -> Build f p
known = return . defWithBody . Known

-- |Refers to the column being built, binding it for use as a tuple.
refer :: Build f p -> MkCol f p (Tuple f)
refer build = finishBuild build >>= joining

-- |Lifts a tuple to a column.
lift :: Tuple f -> Build f p
lift = return . defWithBody . Lift


-- |Orders the column being built by fields specified by the order.
-- *Should be applied at last*, as its effect does not leak out from inside join.
-- Only outmost one would be in effect when applying order twice.
-- Without window, comes after partition. With window, comes before partition.
order :: [Order] -> Build f p -> Build f p
order ords = fmap (\x -> x{ colOrd = ords }) . finishBuild

-- |Partition
-- Aggregates over entire column when empty list is passed to part index.
-- Should be applied as last, as its effect does not leak out from inside join.
-- Only outmost one would be in effect when applying partition twice.
partitions :: [PartIndex] -> [(String, p)] -> Build f p -> Build f p
partitions index ag = fmap (\x -> x{ colPart = Just (index, ag) }) . finishBuild

-- TODO Interaction with window & union
-- |Window function
-- Works in-place, alike join.
-- Its entire purpose is for aggregates,
-- so it is no-op when not coupled with partition "inside".
window :: RowPos -> RowPos -> Build f p -> Build f p
window pri post = fmap (\x -> x{ colWindow = (pri, post) <$ colPart x }) . finishBuild


-- TODO How to handle Union-After-Order: Join?
-- |Union
-- Discards any transformations in each part, e.g. order, partition, window.
-- Refer and join the part to preserve these operations.
union :: Build f p -> Build f p -> Build f p
union = liftA2 (fmap defWithBody . Combine Union `on` colBody) `on` finishBuild

-- |Intersection
-- Discards any transformations in each part, e.g. order, partition, window.
-- Refer and join the part to preserve these operations.
intersection :: Build f p -> Build f p -> Build f p
intersection = liftA2 (fmap defWithBody . Combine Intersect `on` colBody) `on` finishBuild

-- |Difference
-- Discards any transformations in each part, e.g. order, partition, window.
-- Refer and join the part to preserve these operations.
difference :: Build f p -> Build f p -> Build f p
difference = liftA2 (fmap defWithBody . Combine Diff `on` colBody) `on` finishBuild

infixr 2 `union`
infixr 3 `intersection`
infixr 3 `difference`