module Data.Column (
  module Data.RefTuple
  , module Data.Impl.Column
  , module Data.Impl.Classes
  , module Data.Impl.Singles
) where

import Data.RefTuple (
  Tuple(..), Single(..), select
  , singular, singularTag
  )
import Data.Impl.Column (
  Column, RowPos(..), Final, known, MkCol
  , buildCol, refer, lift, partition, window
  )
import Data.Impl.Classes ( With(..), With1(..) )
import Data.Impl.Singles
import Data.Impl.Aggregates

example1 :: (Basics f, Numeric f) => Single f -> Column f p
example1 ident = buildCol $ do
  c <- refer $ known "Class"
  wherein $ c:."priority" :>: wrapI 0 `Cand` c:."course" :=: ident
  t <- refer $ known "Time"
  wherein $ c:."sem" :=: t:."sem"
  let ord = OrderBy (c:."begin") `Chain` (Rev . OrderBy) (c:."end")
  order ord . lift $ select ["course", "begin", "end"] c

example2 :: (Basics f, BasicAggs p) => Column f p
example2 = buildCol $ do
  c <- refer $ known "Class"
  let ident = select ["ident"] c
  let ord = OrderBy (c:."ident")
  -- TODO ident passing?
  partition ident (const ccount) . order ord
    . lift $ select ["ident", ""] c