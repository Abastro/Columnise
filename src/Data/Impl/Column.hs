{-# LANGUAGE GADTSyntax #-}
module Data.Impl.Column (
  OrdDir(..), CombineOp(..), RowPos(..), orders
  , Column(..), ColBody(..), ColProps
  , Transforms(..), filters, sorts, ranged
  , Win, Aggregative(..), TrCol(..)
  , JoinCol, builds, joining
  , ListCol, ZipCol(..), viewWith, collapse
  , known, refer, wherein, asCol, union, intersection, difference
  , withTr, groups, windows
) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont
import Data.Proxy
import Data.Functor.Compose ( Compose(..) )
import qualified Data.Map as M

import Data.Impl.Utils
import Data.Impl.Operable
import Data.Impl.Named
import Data.Impl.Classes

-- TODO Insert, Update, Delete
-- TODO Support on left joins and others

-- |Ordering directions
data OrdDir = Asc | Dsc deriving Eq
-- |Combine operations
data CombineOp = Union | Intersect | Diff deriving Eq
-- |Relative row position
data RowPos = MinPos | RelPos Int | MaxPos deriving (Eq, Ord)

-- |Non-inhabited data to denote order.
data Order
-- |Gives order using certain field.
orders :: Vals f a -> OrdDir -> (OrdDir, Vals f Order)
orders (Vals v) r = (r, Vals v) -- MAYBE add order-able constraint here?
infix 1 `orders`

-- |Represents column body; Variables are represented by Int
data ColBody f where
  Known :: String -> ColBody f
  Lift :: TupRaw f -> ColBody f
  Where :: Operable f -> ColBody f
  Join :: Int -> ColumnRaw f -> ColBody f -> ColBody f
  Combine :: CombineOp -> ColBody f -> ColBody f -> ColBody f

indVar = 0 -- Variable used for indices(partition) / reference
aggVar = -1 -- Variable used for target being aggregated

-- |Denotes transformations for the column
data Transforms f a = Transforms{
  trFilter :: Maybe (Operable f)
  , trSort :: Maybe [(OrdDir, Operable f)]
  , trRange :: Maybe (RowPos, RowPos)
} deriving Functor
instance Semigroup (Transforms f a) where
  Transforms f s r <> Transforms f' s' r' = Transforms (f <|> f') (s <|> s') (r <|> r')
instance Monoid (Transforms f a) where
  mempty = Transforms Nothing Nothing Nothing

-- | /Filter transformation./
--
-- Analogous to the where statement, but for groups/windows.
--
-- For window, applied the last. Otherwise, recommended to be the first.
filters :: TupleNormal l f => (Tuple l -> Vals f Bool) -> Transforms f (Tuple l)
filters f = mempty{ trFilter = Just . getVals $ f (referred indVar) }

-- | /Sort transformation./
--
-- Should come before range.
-- TODO Represent order from Vals
sorts :: TupleNormal l f => (Tuple l -> [(OrdDir, Vals f Order)]) -> Transforms f (Tuple l)
sorts f = mempty{ trSort = Just $ second getVals <$> f (referred indVar) }

-- | /Transformation of restricting range./
--
-- When applied, only rows within the range are left selected.
-- Should come after sort.
ranged :: RowPos -> RowPos -> Transforms f a
ranged pre fol = mempty{ trRange = Just (pre, fol) }

-- |Denotes certain window
type Win = Int

-- |Represents aggregating process
data Aggregative f k = Aggregative{
  partMap :: M.Map k (TupRaw f, Transforms f ())
  , aggregates :: [(k, Aggregate f (Operable f))]
}
instance Ord k => Semigroup (Aggregative f k) where
  Aggregative m a <> Aggregative m' a' = Aggregative (m <> m') (a <> a')
instance Ord k => Monoid (Aggregative f k) where
  mempty = Aggregative M.empty []

-- |Represents column transformation
data TrCol f =
  ColTrans (Transforms f ())
  | ColGroup (Aggregative f ()) ([Operable f] -> TupRaw f)
  | ColWindow (Aggregative f Win) ([Operable f] -> TupRaw f)

-- |Column property
type ColProps f = WithProp [TrCol f]

-- |Raw column, used for implementations. Left of tuple denotes fields returned
type ColumnRaw f = ColProps f ([String], ColBody f)

-- |General column
newtype Column f a = Column{ runColumn :: Fresh Int (ColumnRaw f) }
-- Perhaps use continuation here?

-- |Monad for column joins
newtype JoinCol f a = JoinCol (ContT (ColBody f) (Fresh Int) a)
  deriving (Functor, Applicative, Monad) via (ContT (ColBody f) (Fresh Int))

inColumn :: TupleNormal l f => Proxy (Tuple l) -> Fresh Int (ColBody f) -> Column f (Tuple l)
inColumn p b = Column $ fromBody . (fields p, ) <$> b


-- |Implementation-only function.
-- Builds the join-column of tuple into column body.
builds :: TupleNormal l f => JoinCol f (Tuple l) -> Fresh Int (ColBody f)
builds (JoinCol b) = runContT b (pure . Lift . asRaw)

-- |Implementation-only function.
-- Joins (& binds) given column.
joining :: ColumnRaw f -> JoinCol f Int
joining raw = JoinCol $ do
  index <- lift fresh
  -- Adds join statement to body from the continuation
  ContT $ \next -> procJoin index raw <$> next index
  where
    -- Uses associativity of join to reduce nesting
    procJoin j RunProp{ prop = [], body = (cf, Join k ci bi) } tb =
      procJoin k ci . procJoin j (fromBody (cf, bi)) $ tb
    -- Removes unnecessary indirection
    procJoin j RunProp{ prop = [], body = (cf, b) } (Lift t) | isSameRef cf j t =
      b
    procJoin j c tb = Join j c tb
    -- Check if t is same with certain reference
    isSameRef fs n t = Just ((, n) <$> fs) == traverse ref t
    ref (n, Refer j nf) | n == nf = Just (n, j)
    ref _ = Nothing

-- |Represents column of list to aggregate
data ListCol f k a = ListCol (TupRaw f, Transforms f ()) a
  deriving Functor
-- |Internal constructor for listcol
mkListCol :: (TupleNormal l f, TupleNormal m f) => (Tuple l -> Tuple m) ->
  Transforms f (Tuple l) -> ListCol f k (Tuple l)
mkListCol indices trans = ListCol
  (asRaw $ indices (referred indVar), () <$ trans) (referred aggVar)

-- |Applicative for column zips, used for group and window
newtype ZipCol f k a = ZipCol (
  WriterT (Aggregative f k) (Fresh k) (Fresh (Operable f) a) )
  deriving (Functor, Applicative) via
  (Compose (WriterT (Aggregative f k) (Fresh k)) (Fresh (Operable f)))
-- |Internal runs for zipcol
runZipCol :: [k] -> ZipCol f k (TupRaw f) -> ([Operable f] -> TupRaw f, Aggregative f k)
runZipCol ins (ZipCol zc) = (`runFresh` ins) $ runWriterT . fmap runFresh $ zc

-- |Implementation-only function. Applies column transformation.
applyTrCol :: TrCol f -> Column f a -> Column f b
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


-- |References a known table, i.e. a stored table.
known :: TupleNormal l f => String -> Column f (Tuple l)
known = inColumn Proxy . pure . Known

-- |Refers to a column as a join-column of tuples.
refer :: TupleNormal l f => Column f (Tuple l) -> JoinCol f (Tuple l)
refer (Column col) = JoinCol (lift col) >>= (fmap referred . joining)

-- |Represents where statement.
wherein :: Vals f Bool -> JoinCol f ()
wherein cond = () <$ joining (pure ([], Where $ getVals cond))

-- |Turns the join-column into a column
asCol :: TupleNormal l f => JoinCol f (Tuple l) -> Column f (Tuple l)
asCol jc = inColumn Proxy (builds jc)

-- |Union of the join-columns
union :: TupleNormal l f => JoinCol f (Tuple l) -> JoinCol f (Tuple l) -> JoinCol f (Tuple l)
union = fmap (refer . inColumn Proxy) . liftA2 (Combine Union) `on` builds

-- |Intersection of the join-columns
intersection :: TupleNormal l f => JoinCol f (Tuple l) -> JoinCol f (Tuple l) -> JoinCol f (Tuple l)
intersection = fmap (refer . inColumn Proxy) . liftA2 (Combine Intersect) `on` builds

-- |Gets difference of the join-columns
difference :: TupleNormal l f => JoinCol f (Tuple l) -> JoinCol f (Tuple l) -> JoinCol f (Tuple l)
difference = fmap (refer . inColumn Proxy) . liftA2 (Combine Diff) `on` builds

infixr 2 `union`
infixr 3 `intersection`
infixr 3 `difference`


-- |Collapses the aggregated column of list into column zips
collapse :: (Functor (Aggregate f), Ord k) =>
  ListCol f k (Aggregate f (Vals f a)) -> ZipCol f k (Vals f a)
collapse (ListCol part agg) = ZipCol $ do
  k <- lift fresh -- Fresh name for window
  tell Aggregative{ partMap = M.singleton k part, aggregates = [(k, getVals <$> agg)] }
  return $ Vals <$> fresh -- Returns *current aggregated result*, which is bound as parameter

-- | /Applies a transformation to the column./
--
-- No-op when directly applied to a lift.
withTr :: Transforms f a -> (Column f a -> Column f a)
withTr tr = applyTrCol (ColTrans $ () <$ tr)

-- | /Applies grouping to the column./
--
-- Group gives aggregated results from the partition with given indices.
--
-- Function scheme: @indices -> aggr -> result@
--
-- [indices]: Denotes grouping indices deduced from the tuple.
-- [aggr]: Denotes grouping to attain the result of 'groups'.
--  * First parameter of @aggr@ denotes the indices, which can be used in result.
--  * Second parameter of @aggr@ denotes the lists to be aggregated.
-- [result]: Gives column transformation by the grouping.
groups :: (TupleNormal l f, TupleNormal m f, TupleNormal n f) =>
  (Tuple l -> Tuple m) ->
  (ZipCol f () (Tuple m) -> ListCol f () (Tuple l) -> ZipCol f () (Tuple n)) ->
  (Column f (Tuple l) -> Column f (Tuple n))
groups indices aggr = applyTrCol (ColGroup pre post) where
  grouped = aggr (pure $ referred indVar) (mkListCol indices mempty)
  (post, pre) = runZipCol (repeat ()) $ asRaw <$> grouped

-- |View the window partitioned by indices tuple with given transformation,
-- which is to be aggregated later.
viewWith :: (TupleNormal l f, TupleNormal m f) => (Tuple l -> Tuple m) ->
  Transforms f (Tuple l) -> ListCol f Win (Tuple l)
viewWith = mkListCol

-- | /Applies windows to the column./
--
-- Window gives locally aggregated result for each row.
-- e.g. it can be used to calculate sum until certain rows.
--
-- It is recommended to apply window after filter transformation.
--
-- Function scheme: @zips -> result@
--
-- [zips]: Denotes zipping to attain the result of 'windows'.
--  Parameter of @zips@ denote the original tuples.
--  Use 'viewWith' to aggregate over a window.
-- [result]: Gives column transformation by the windows.
windows :: (TupleNormal l f, TupleNormal m f) =>
  (ZipCol f Win (Tuple l) -> ZipCol f Win (Tuple m)) ->
  (Column f (Tuple l) -> Column f (Tuple m))
windows zips = applyTrCol (ColWindow pre post) where
  (post, pre) = runZipCol [1..] $ asRaw <$> zips (pure $ referred indVar)
