{-# LANGUAGE GADTSyntax #-}
module Data.Impl.Column (
  Column, ColBody(..), ColProps, CProps(..), isCPEmpty
  , Omits(..), omit, OrdDir(..), Order, asc, dsc, CombineOp(..), RowPos(..)
  , MkCol, Build, BuiltColumn, build, joining, known, refer, lift
  , order, union, intersection, difference, partitions, window
) where

import Control.Applicative
import Control.Monad.Trans.Cont
import Control.Monad.Writer hiding ( lift )
import qualified Control.Monad.Trans as T
import Data.Function ( on )
import Data.Maybe

import Data.Impl.Utils
import Data.Impl.Classes
import Data.RefTuple ( Tuple(..), Single(..) )

-- TODO Insert, Update, Delete
-- TODO Typecheck column statements

-- |Denotes omittable
data Omits a = Omits { omits :: Bool, ins :: a } deriving (Eq, Functor)
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

data CombineOp = Union | Intersect | Diff deriving Eq
-- |Relative row position
data RowPos = MinPos | RelPos Int | MaxPos deriving (Eq, Ord)

data ColBody f where
  Known :: String -> ColBody f
  Lift :: Tuple f -> ColBody f
  Where :: Single f -> ColBody f
  -- MAYBE De-bruijin indexing could make this int obsolete
  Join :: Int -> Column f -> ColBody f -> ColBody f
  Combine :: CombineOp -> ColBody f -> ColBody f -> ColBody f

type ColProps f = WithProp (CProps f)
data CProps f = CProps{
  cOrd :: [Order]
  -- Keys to apply partition by, aggregate selection
  , cPart :: Maybe ([String], [(String, Aggregate f)])
  , cWindow :: Maybe (RowPos, RowPos)
}
-- Not lawful, but serves purpose of limiting use of window
instance Semigroup (CProps f) where
  CProps r p w <> CProps r' p' w' = CProps r'' p'' $ p'' *> (w <|> w')
    where r'' = if null r then r' else r; p'' = p <|> p'
instance Monoid (CProps f) where
  mempty = CProps [] Nothing Nothing

-- |Implementation-only function.
-- Checks if column property is empty.
isCPEmpty :: CProps f -> Bool
isCPEmpty (CProps r p w) = null r && isNothing p && isNothing w

-- |Column data with operation f (Variable is indexed by Int)
type Column f = ColProps f (ColBody f)


newtype Builder r a = Builder (ContT r Fresh a)
  deriving (Functor, Applicative, Monad)
type BdBody f = Builder (ColBody f)
type BdCol f = Builder (Column f)

-- |Monad for column construction.
-- Bind operation joins columns being bound.
newtype MkCol f a = MkCol ((WriterT (Endo (ColBody f)) Fresh) a)
  deriving (Functor, Applicative, Monad)

type Build f = MkCol f (Column f)
type BuiltColumn f = Fresh (Column f)

-- |Implementation-only function.
-- Locally finishes builds, processing joins. Does not affect fresh binding state.
finishBuild :: Build f -> Build f
finishBuild (MkCol build) = MkCol . pass $ do
  (fin, Endo apply) <- listen build
  pure (apply <$> fin, const mempty)

-- |Implementation-only function.
-- Joins (& binds) given column.
joining :: Column f -> MkCol f (Tuple f)
joining col = do
  index <- MkCol $ T.lift fresh
  MkCol . tell $ Endo $ Join index col
  return $ TRef index

-- |Builds the column
build :: Build f -> BuiltColumn f
build build = fmap fst . runWriterT $ form where
  MkCol form = finishBuild build

-- |Refers to the known table
known :: String -> Build f
known = return . fromBody . Known

-- |Refers to the column being built, binding it for use as a tuple.
refer :: Build f -> MkCol f (Tuple f)
refer build = finishBuild build >>= joining

-- |Lifts a tuple to a column.
lift :: Tuple f -> Build f
lift = return . fromBody . Lift


-- |Orders the column being built by fields specified by the order.
-- *Should be applied at last*, as its effect does not leak out from inside join.
-- (Note that, still, joined table is joined with the specified order)
-- Only outmost one would be in effect when applying order twice.
-- Without window, comes after partition. With window, comes before partition.
order :: [Order] -> Build f -> Build f
order ords = fmap (<* fromProp mempty{ cOrd = ords }) . finishBuild

-- |Partition
-- Aggregates over entire column when empty list is passed to part index.
-- *Should be applied as last*, as its effect does not leak out from inside join.
-- Only outmost one would be in effect when applying partition twice.
partitions :: [String] -> [(String, Aggregate f)] -> Build f -> Build f
partitions index ag = fmap (<* fromProp mempty{ cPart = Just (index, ag) }) . finishBuild

-- TODO Interaction with window & union
-- |Window function
-- Works in-place, alike join.
-- Its entire purpose is for aggregates,
-- so it is no-op when not coupled with partition "inside".
window :: RowPos -> RowPos -> Build f -> Build f
window pri post = fmap (<* fromProp mempty{ cWindow = Just (pri, post) }) . finishBuild


-- TODO How to handle Union-After-Order: Join?
-- |Union
-- Discards any transformations in each part, e.g. order, partition, window.
-- Refer and join the part to preserve these operations.
union :: Build f -> Build f -> Build f
union = liftA2 (fmap fromBody . Combine Union `on` body) `on` finishBuild

-- |Intersection
-- Discards any transformations in each part, e.g. order, partition, window.
-- Refer and join the part to preserve these operations.
intersection :: Build f -> Build f -> Build f
intersection = liftA2 (fmap fromBody . Combine Intersect `on` body) `on` finishBuild

-- |Difference
-- Discards any transformations in each part, e.g. order, partition, window.
-- Refer and join the part to preserve these operations.
difference :: Build f -> Build f -> Build f
difference = liftA2 (fmap fromBody . Combine Diff `on` body) `on` finishBuild

infixr 2 `union`
infixr 3 `intersection`
infixr 3 `difference`