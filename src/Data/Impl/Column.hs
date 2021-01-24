{-# LANGUAGE GADTSyntax #-}
module Data.Impl.Column (
  Column(..), CombineOp(..), RowPos(..)
  , Final(..), known, Build, finalProc
  , MkCol, buildCol, mkVar, refer, lift, partition, window
) where

import Control.Monad.Writer ( Writer, runWriter, tell )
import Control.Monad.State ( StateT(..), evalStateT, get, modify )
import Data.Monoid ( Endo(..) )
import Data.Coerce ( coerce )

import Data.RefTuple ( Tuple(..), Single(..) )

data CombineOp = Union | Intersect | Diff
data RowPos = RelPos Int | MinPos | MaxPos

-- |Column data with operation f (Variable is indexed by Int)
-- TODO This requires types for inspecting which field the tuple has
data Column f p where
  Known :: String -> Column f p
  Lift :: Tuple f -> Column f p
  Where :: Single f -> Column f p
  Join :: Int -> Column f p -> Column f p -> Column f p

  Combine :: CombineOp -> Column f p -> Column f p -> Column f p
  Order :: Single f -> Column f p -> Column f p
  Partition :: Tuple f -> (String -> p) -> Column f p -> Column f p
  Window :: RowPos -> RowPos -> Column f p -> Column f p


-- TODO Typecheck column statements
-- TODO Insert, Update, Delete


-- |Denotes final in building column.
-- Provided for more safety, especially to disallow leaking 
newtype Final f p = Final { finalCol :: Column f p }

-- |Refers to the known table
known :: String -> Final f p
known = Final . Known


type MkCol f p = StateT Int (Writer (Endo (Column f p)))
type Build f p = MkCol f p (Final f p)

finalProc :: (Column f p -> Column f p) -> Build f p -> Build f p
finalProc f = (fmap . coerceFn) f where
  coerceFn f x = coerce (f `asTypeOf` (`asTypeOf` finalCol x)) x


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

-- |Lifts a tuple to a column (with single row).
lift :: Tuple f -> Build f p
lift s = return . Final $ Lift s

-- |Partition
partition :: Tuple f -> (String -> p) -> Build f p -> Build f p
partition index ag = finalProc (Partition index ag)

-- |Window function.
-- Discards order and partition statements in the last parameter if it is lifted tuple.
-- This is so because logically 'ordering single column' does not make sense.
-- TODO Actula discard here..
window :: RowPos -> RowPos -> (Build f p -> Build f p) -> Build f p -> Build f p
window pri post win tar = finalProc (Window pri post) $ win tar
