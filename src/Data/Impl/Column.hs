{-# LANGUAGE GADTSyntax #-}
module Data.Impl.Column (
  OrdDir(..), CombineOp(..), RowPos(..)
  , Column(..), ColBody(..), ColProps
  , Transforms(..), filters, sorts, ranged
  , Win, Aggregative(..), TrCol(..)
  , JoinCol, builds, joining
  , ListCol, ZipCol(..), origin, viewWith, collapse
  , known, refer, asCol, union, intersection, difference
  , withTr, groups, windows
) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont
import Data.Function ( on )
import Data.Bifunctor ( Bifunctor(..) )
import Data.Functor.Compose ( Compose(..) )
import qualified Data.Map as M

import Data.Impl.Utils
--import Data.Impl.Named
import Data.Impl.Classes
import Data.Impl.RefTuple ( Tuple(..), Single(..) )

-- TODO Insert, Update, Delete
-- TODO Typecheck column statements
-- TODO OR, way to involve general types

type Tup f = [(String, Single f)]
genRef :: [String] -> Int -> Tup f
genRef fs n = (, undefined n) <$> fs
isSameRef :: CheckField -> Int -> Tup f -> Bool
isSameRef (CkField fs) n t = ((, Just n) <$> fs) == (second undefined <$> t)

-- |Ordering directions
data OrdDir = Asc | Dsc deriving Eq
data CombineOp = Union | Intersect | Diff deriving Eq
-- |Relative row position
data RowPos = MinPos | RelPos Int | MaxPos deriving (Eq, Ord)

-- |Represents column body; Variables are represented by Int
data ColBody f where
  Known :: String -> ColBody f
  Lift :: Tuple f -> ColBody f
  Where :: Single f -> ColBody f
  -- MAYBE De-bruijin indexing could make this int obsolete
  Join :: Int -> ColumnRaw f -> ColBody f -> ColBody f
  Combine :: CombineOp -> ColBody f -> ColBody f -> ColBody f

indVar = 0 -- Variable used for indices(partition) / reference
aggVar = -1 -- Variable used for target being aggregated

-- |Denotes transformations for the column
data Transforms f = Transforms{
  trFilter :: Maybe (Single f)
  , trSort :: Maybe [(OrdDir, Single f)]
  , trRange :: Maybe (RowPos, RowPos)
}
instance Semigroup (Transforms f) where
  Transforms f s r <> Transforms f' s' r' = Transforms (f <|> f') (s <|> s') (r <|> r')
instance Monoid (Transforms f) where
  mempty = Transforms Nothing Nothing Nothing

-- |Filter transformation.
-- Analogous to the where statement, but for groups/windows.
-- For window, applied the last. Otherwise, recommended to be the first.
filters :: (Tuple f -> Single f) -> Transforms f
filters f = mempty{ trFilter = Just $ f (TRef indVar) }

-- |Sort transformation.
-- Should come before range.
sorts :: (Tuple f -> [(OrdDir, Single f)]) -> Transforms f
sorts f = mempty{ trSort = Just $ f (TRef indVar) }

-- |Restricting range transformation.
-- When applied, only rows within the range are left selected. Also used on window.
-- Should come after sort.
ranged :: RowPos -> RowPos -> Transforms f
ranged pre fol = mempty{ trRange = Just (pre, fol) }

-- |Denotes certain window
type Win = Int

-- |Represents aggregating process
data Aggregative f k = Aggregative{
  partMap :: M.Map k (Tuple f, Transforms f)
  , aggregates :: [(k, Aggregate f (Single f))]
}
instance Ord k => Semigroup (Aggregative f k) where
  Aggregative m a <> Aggregative m' a' = Aggregative (m <> m') (a <> a')
instance Ord k => Monoid (Aggregative f k) where
  mempty = Aggregative M.empty []

-- |Represents column transformation
data TrCol f =
  ColTrans (Transforms f)
  | ColGroup (Aggregative f ()) ([Single f] -> Tuple f)
  | ColWindow (Aggregative f Win) ([Single f] -> Tuple f)

-- |Column property
type ColProps f = WithProp [TrCol f]

-- Internal machinery to check fields match
newtype CheckField = CkField [String]
instance Semigroup CheckField where
  CkField x <> CkField y = if x == y then CkField x else error "impossible, field mismatch"
instance Monoid CheckField where -- Required due to tuple applicative
  mempty = error "impossible, lacks field specification. Likely where statement"
-- |Raw column, used for implementations. Left of tuple denotes fields returned
type ColumnRaw f = ColProps f (CheckField, ColBody f)

-- |General column
newtype Column f = Column{ runColumn :: Fresh Int (ColumnRaw f) }
-- Perhaps use continuation here?

-- |Monad for column joins
newtype JoinCol f a = JoinCol (ContT (CheckField, ColBody f) (Fresh Int) a)
  deriving (Functor, Applicative, Monad) via (ContT (CheckField, ColBody f) (Fresh Int))

inColumn = Column . fmap fromBody -- Wraps join-column body into a column

-- |Implementation-only function.
-- Builds the join-column of tuple into column body.
builds :: JoinCol f (Tuple f) -> Fresh Int (CheckField, ColBody f)
builds (JoinCol b) = runContT b (pure . withCk) where
  withCk t = (undefined t, Lift t)

-- |Implementation-only function.
-- Joins (& binds) given column.
joining :: ColumnRaw f -> JoinCol f (Tuple f)
joining raw = JoinCol $ do
  index <- lift fresh
  -- Adds join statement to body from the continuation
  ContT $ \next -> procJoin index raw <$> next (TRef index)
  where
    -- Uses associativity of join to reduce nesting
    procJoin j RunProp{ prop = [], body = (cf, Join k ci bi) } tb =
      procJoin k ci . procJoin j (fromBody (cf, bi)) $ tb
    -- Removes unnecessary indirection
    procJoin j RunProp{ prop = [], body = (cf, b) } (_, Lift t) | isSameRef cf j undefined =
      (cf, b)
    procJoin j c tb = Join j c <$> tb

-- |Represents column of list to aggregate
data ListCol f k a = ListCol (Tuple f, Transforms f) a
  deriving Functor
-- |Implementation-only constructor for listcol
mkListCol :: (Tuple f -> Tuple f) -> Transforms f -> ListCol f k (Tuple f)
mkListCol indices trans = ListCol (indices (TRef indVar), trans) (TRef aggVar)

-- |Applicative for column zips, used for group and window
newtype ZipCol f k a = ZipCol{
  runZipCol :: WriterT (Aggregative f k) (Fresh k) (Fresh (Single f) a)
} deriving (Functor, Applicative) via
  (Compose (WriterT (Aggregative f k) (Fresh k)) (Fresh (Single f)))

-- |The origin data to look up for.
-- For group, this is the index tuple of partition.
-- For window, this is the original tuple.
origin :: Ord k => ZipCol f k (Tuple f)
origin = pure (TRef indVar)

-- |View the 'window' partitioned by indices tuple with given transformation,
-- which is to be aggregated later.
viewWith :: (Tuple f -> Tuple f) -> Transforms f -> ListCol f Win (Tuple f)
viewWith = mkListCol

-- |Collapses the aggregated column of list into column zips
collapse :: Ord k => ListCol f k (Aggregate f (Single f)) -> ZipCol f k (Single f)
collapse (ListCol part agg) = ZipCol $ do
  k <- lift fresh -- Fresh name for window
  tell Aggregative{ partMap = M.singleton k part, aggregates = [(k, agg)] }
  return fresh -- Returns *current aggregated result*, which is bound as parameter

-- |Implementation-only function. Applies column transformation.
applyTrCol :: TrCol f -> Column f -> Column f
applyTrCol tr (Column col) = Column $ first (apply . (tr :)) <$> col where
  -- merges transformation with order
  mergeTr (Transforms Nothing Nothing Nothing) (Transforms f s r) =
    [Transforms f s r]
  mergeTr (Transforms Nothing Nothing r) (Transforms f s Nothing) =
    [Transforms f s r]
  mergeTr (Transforms Nothing s r) (Transforms f _ Nothing) =
    [Transforms f s r] -- Trivially nullifies previous sort
  mergeTr (Transforms f s r) (Transforms Nothing Nothing Nothing) =
    [Transforms f s r]
  mergeTr t t' = [t, t']

  apply (ColTrans t : ColTrans x : xs) = map ColTrans (mergeTr t x) <> xs
  apply l = l


-- |Refers to the known table
known :: String -> Column f
known = inColumn . pure . (undefined, ) . Known where todo = ""

-- |Refers to a column as a join-column of tuples.
refer :: Column f -> JoinCol f (Tuple f)
refer (Column col) = JoinCol (lift col) >>= joining

-- |Turns the join-column into a column
asCol :: JoinCol f (Tuple f) -> Column f
asCol = inColumn . builds


-- |Union of the join-columns
union :: JoinCol f (Tuple f) -> JoinCol f (Tuple f) -> JoinCol f (Tuple f)
union = fmap (refer . inColumn) . (liftA2 . liftA2) (Combine Union) `on` builds

-- |Intersection of the join-columns
intersection :: JoinCol f (Tuple f) -> JoinCol f (Tuple f) -> JoinCol f (Tuple f)
intersection = fmap (refer . inColumn) . (liftA2 . liftA2) (Combine Intersect) `on` builds

-- |Gets difference of the join-columns
difference :: JoinCol f (Tuple f) -> JoinCol f (Tuple f) -> JoinCol f (Tuple f)
difference = fmap (refer . inColumn) . (liftA2 . liftA2) (Combine Diff) `on` builds

infixr 2 `union`
infixr 3 `intersection`
infixr 3 `difference`


-- |Applies a transformation to the column.
-- No-op when directly applied to a lift.
withTr :: Transforms f -> Column f -> Column f
withTr tr = applyTrCol (ColTrans tr)

-- |Applies group to the column.
-- Group gives aggregated results for the provided partition from the indices.
groups :: (Tuple f -> Tuple f) ->
  (ListCol f () (Tuple f) -> ZipCol f () (Tuple f)) -> (Column f -> Column f)
groups indices aggr = applyTrCol (ColGroup pre post) where
  (post, pre) = (`runFresh` repeat ()) . runWriterT . fmap runFresh . runZipCol
    $ aggr (mkListCol indices mempty)

-- |Applies windows to the column.
-- Window gives locally aggregated result for each row.
-- e.g. it can be used to calculate sum until certain rows.
-- Note that the transformations are applied to the local list for aggregates.
-- It is recommended to apply window after filter transformation.
windows :: ZipCol f Win (Tuple f) -> (Column f -> Column f)
windows zipped = applyTrCol (ColWindow pre post) where
  (post, pre) = (`runFresh` [1..]) . runWriterT . fmap runFresh . runZipCol $ zipped

