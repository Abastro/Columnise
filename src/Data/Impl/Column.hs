{-# LANGUAGE GADTSyntax #-}
module Data.Impl.Column (
  Column(..), Order(..), CombineOp(..), RowPos(..)
  , Final(..), known, Build, liftBuild, liftBuild2
  , MkCol, buildCol, mkVar, refer, lift
  , order, union, intersection, difference, partition, window
) where

import Control.Applicative ( Applicative(..) )
import Control.Monad.Writer ( Writer, runWriter, tell )
import Control.Monad.State ( StateT(..), evalStateT, get, modify )
import Data.Monoid ( Endo(..) )
import Data.Coerce ( coerce )

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


-- |Denotes final in building column.
-- Provided for more safety, especially to disallow leaking 
newtype Final f p = Final { finalCol :: Column f p }

-- |Refers to the known table
known :: String -> Final f p
known = Final . Known


type MkCol f p = StateT Int (Writer (Endo (Column f p)))
type Build f p = MkCol f p (Final f p)

liftBuild :: (Column f p -> Column f p) -> Build f p -> Build f p
liftBuild f = fmap . coerceFn $ f where
  coerceFn f x = coerce (f `asTypeOf` (`asTypeOf` finalCol x)) x

liftBuild2 :: (Column f p -> Column f p -> Column f p) -> Build f p -> Build f p -> Build f p
liftBuild2 f = liftA2 . coerceFn $ f where
  coerceFn f x = coerce (f `asTypeOf` const (`asTypeOf` finalCol x)) x


-- |Creates a variable
mkVar :: MkCol f p Int
mkVar = get <* modify succ

-- |Builds the column
buildCol :: Build f p -> Column f p
buildCol form = apply fin
  where (Final fin, Endo apply) = runWriter . (`evalStateT` 1) $ form

-- |Refers to the (final) column, binding it for use as a tuple.
refer :: Final f p -> MkCol f p (Tuple f)
refer (Final col) = do
  index <- mkVar
  tell $ Endo $ Join index col
  return $ TRef index

-- |Lifts a tuple to a column.
lift :: Tuple f -> Build f p
lift s = return . Final $ Lift s


-- |Gives ordered column. Only the last order statement gets in effect.
order :: [Order] -> Build f p -> Build f p
order ords = liftBuild $ Order ords

-- TODO How to handle Union-After-Order
-- |Union
union :: Build f p -> Build f p -> Build f p
union = liftBuild2 $ Combine Union

-- |Intersection
intersection :: Build f p -> Build f p -> Build f p
intersection = liftBuild2 $ Combine Intersect

-- |Difference
difference :: Build f p -> Build f p -> Build f p
difference = liftBuild2 $ Combine Diff


-- |Partition
partition :: Tuple f -> [(String, p)] -> Build f p -> Build f p
partition index ag = liftBuild (Partition index ag)

-- |Window function.
-- Discards order and partition statements in the last parameter if it is lifted tuple.
-- This is so because logically 'ordering single column' does not make sense.
-- TODO Actula discard here..
window :: RowPos -> RowPos -> (Build f p -> Build f p) -> Build f p -> Build f p
window pri post win tar = liftBuild (Window pri post) $ win tar
