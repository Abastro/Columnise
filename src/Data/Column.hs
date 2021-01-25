module Data.Column (
  module Data.RefTuple
  , module Data.Impl.Column
  , module Data.Impl.Classes
  , module Data.Impl.Singles
) where

import Data.RefTuple (
  Tuple(..), Single(..), as, select
  , singular, singularTag
  )
import Data.Impl.Column (
  Column, Order(..), RowPos(..), Final, known
  , MkCol, Build, buildCol, refer, lift
  , order, union, intersection, difference, partition, window
  )
import Data.Impl.Classes ( With(..), With1(..) )
import Data.Impl.Singles
import Data.Impl.Aggregates

-- TODO Why are examples here..
example1 :: (WithCond f, Numeric f) => Single f -> Column f p
example1 ident = buildCol $ do
  c <- refer $ known "Class"
  wherein $ c:."priority" :>=: wrapI 0 `Cand` c:."course" :=: ident
  t <- refer $ known "Time"
  wherein $ c:."sem" :=: t:."sem"
  order [Asc "begin", Dsc "end"] . lift $ select ["course", "begin", "end"] c

example2 :: (WithCond f, Aggregates p) => Column f p
example2 = buildCol $ do
  c <- refer $ known "Class"
  t <- refer $ known "Time"
  wherein $ c:."sem" :=: t:."sem"
  order [Dsc "numClasses"] . partition (select ["ident"] c) [
    "ident" `indexAs` "class"
    , ccount `as` "numClasses"
    , cmin "begin" `as` "first"
    , cmax "begin" `as` "last"
    ] $ lift c
