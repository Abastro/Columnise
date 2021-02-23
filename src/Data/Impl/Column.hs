{-# LANGUAGE GADTSyntax #-}
module Data.Impl.Column (
  Column, ColBody(..), ColProps, CProps, isCPEmpty
  , OrdDir(..), CombineOp(..), RowPos(..)
  , JoinCol
  , joining, known, refer, asCol
  , union, intersection, difference
) where

import Control.Applicative
import Control.Monad.State hiding ( lift )
import Control.Monad.Trans.Cont
import qualified Control.Monad.Trans as T
import Data.Function ( on )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Identity ( Identity(..) )

import Data.Impl.Utils
import Data.Impl.Classes
import Data.Impl.RefTuple ( Tuple(..), Single(..) )

-- TODO Insert, Update, Delete
-- TODO Typecheck column statements

-- |Ordering directions
data OrdDir = Asc | Dsc deriving Eq
data CombineOp = Union | Intersect | Diff deriving Eq
-- |Relative row position
data RowPos = MinPos | RelPos Int | MaxPos deriving (Eq, Ord)

-- |Denotes transformations for the column
data Transforms f = Transforms{
  -- |Filter transformation
  trFilter :: Maybe (Single f)
  -- |Sort transformation
  , trSort :: Maybe [(OrdDir, Single f)]
  -- |Range transformation
  , trRange :: Maybe (RowPos, RowPos)
}
instance Semigroup (Transforms f) where
  Transforms f s r <> Transforms f' s' r' = Transforms (f <|> f') (s <|> s') (r <|> r')
instance Monoid (Transforms f) where
  mempty = Transforms Nothing Nothing Nothing

-- |Represents column of list to aggregate
newtype ListCol a = ListCol { runListCol :: a }
  deriving (Functor, Applicative) via Identity

newtype ZipCol w s a = ZipCol (w, State [s] a)
  deriving (Functor, Applicative) via (Compose ((,) w) (State [s]))


-- |Denotes aggregating in group/window
--data AggArg f s = AggArg{
  -- |Aggregate partitions
--  aggPart :: Maybe (PartArg f s)
--}

-- |Constructs aggregating argument, i.e. partition of group or window
--aggregates :: (Tuple f -> PartArg f s) -> AggArg f s
--aggregates f = mempty{ aggPart = Just $ f (TRef 0) }


-- |Filter transformation.
-- Analogous to the where statement, but for groups/windows.
-- For groups, applied the first. For window, applied the last.
filters :: (Tuple f -> Single f) -> Transforms f
filters f = mempty{ trFilter = Just $ f (TRef 0) }

-- |Sort transformation.
-- Comes before range.
sorts :: (Tuple f -> [(OrdDir, Single f)]) -> Transforms f
sorts f = mempty{ trSort = Just $ f (TRef 0) }

-- |Restricting range transformation.
-- When applied, only rows within the range are left selected. Also used on window.
-- Comes after sort.
ranged :: RowPos -> RowPos -> Transforms f
ranged pre fol = mempty{ trRange = Just(pre, fol) }

type CProps f = [Transforms f]


-- |Represents column body; Variables are represented by Int
data ColBody f where
  Known :: String -> ColBody f
  Lift :: Tuple f -> ColBody f
  Where :: Single f -> ColBody f
  -- MAYBE De-bruijin indexing could make this int obsolete
  Join :: Int -> ColumnRaw f -> ColBody f -> ColBody f
  Combine :: CombineOp -> ColBody f -> ColBody f -> ColBody f

type ColProps f = WithProp (CProps f)

-- |Implementation-only function.
-- Checks if column property is empty.
isCPEmpty :: CProps f -> Bool
isCPEmpty props = undefined

-- |Raw column, used for implementations
type ColumnRaw f = ColProps f (ColBody f)

-- |General column
newtype Column f = Column{ runColumn :: Fresh (ColumnRaw f) }

-- |Monad for column joins
newtype JoinCol f a = JoinCol (ContT (ColBody f) Fresh a)
  deriving (Functor, Applicative, Monad) via (ContT (ColBody f) Fresh)

--foo :: Field f => Aggregate f (ListCol (Single f)) -> BdWin f (Single f)
--foo a = BuildWith . ([runListCol <$> a], ) $ gets head <* modify tail

-- |Implementation-only function.
-- Wraps join-column body into a column.
inColumn :: Fresh (ColBody f) -> Column f
inColumn = Column . fmap fromBody

-- |Implementation-only function.
-- Builds the join-column of tuple into column body.
builds :: JoinCol f (Tuple f) -> Fresh (ColBody f)
builds (JoinCol b) = simplify <$> runContT b (pure . Lift) where
  -- General simplification
  simplify cb = case cb of
    Join j c@RunProp{ body = Join k ci bi } b | isCPEmpty (prop c) ->
      simplify $ Join k ci $ Join j (fromBody bi) b -- Uses associativity of join
    Join j c (Lift (TRef k)) | isCPEmpty (prop c) && j == k ->
      body c -- If directly returning what's joined, removes the indirection
    Combine m b b' -> Combine m (simplify b) (simplify b')
    _ -> cb -- Otherwise, do not simplify

-- |Implementation-only function.
-- Joins (& binds) given column.
joining :: ColumnRaw f -> JoinCol f (Tuple f)
joining raw = JoinCol $ do
  index <- T.lift fresh
  -- Adds join statement to body from the continuation
  ContT $ \next -> Join index raw <$> next (TRef index)

-- |Refers to the known table
known :: String -> Column f
known = inColumn . pure . Known

-- |Refers to a column as a join-column of tuples.
refer :: Column f -> JoinCol f (Tuple f)
refer (Column col) = JoinCol (T.lift col) >>= joining

-- |Turns the join-column into a column
asCol :: JoinCol f (Tuple f) -> Column f
asCol = inColumn . builds


-- |Union of the join-columns
union :: JoinCol f (Tuple f) -> JoinCol f (Tuple f) -> JoinCol f (Tuple f)
union = fmap (refer . inColumn) . liftA2 (Combine Union) `on` builds

-- |Intersection of the join-columns
intersection :: JoinCol f (Tuple f) -> JoinCol f (Tuple f) -> JoinCol f (Tuple f)
intersection = fmap (refer . inColumn) . liftA2 (Combine Intersect) `on` builds

-- |Gets difference of the join-columns
difference :: JoinCol f (Tuple f) -> JoinCol f (Tuple f) -> JoinCol f (Tuple f)
difference = fmap (refer . inColumn) . liftA2 (Combine Diff) `on` builds

infixr 2 `union`
infixr 3 `intersection`
infixr 3 `difference`


-- |Applies an argument to the column, to apply transformation or grouping.
-- No-op when directly applied to a lift.
--withArg :: CGroup f -> BuildCol f -> BuildCol f
--withArg grp = fmap (<* fromProp (grp, mempty))

-- TODO How to handle other columns when window is added?
-- TODO Further processing after aggregating?
-- TODO Likely, total redesign

-- |Adds window(s) to the column.
-- Window gives locally aggregated result for each row.
-- e.g. it can be used to calculate sum until certain rows.
-- Note that the transformations are applied to the local list for aggregates.
--addWindow :: [CWindow f] -> BuildCol f -> BuildCol f
--addWindow wins = fmap (<* fromProp (mempty, wins))
