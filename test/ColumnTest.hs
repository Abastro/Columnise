module ColumnTest ( main ) where

import Data.Column

main :: IO ()
main = putStrLn "Test suite not yet implemented."

example1 :: (WithCond f, Numeric f) => Single f -> Column f p
example1 ident = buildCol $ order [Asc "begin", Dsc "end"] $ do
  c <- refer $ known "Class"
  wherein $ c:."priority" :>=: wrapI 0 `Cand` c:."course" :=: ident
  t <- refer $ known "Time"
  wherein $ c:."sem" :=: t:."sem"
  lift $ select ["course", "begin", "end"] c

example2 :: (WithCond f, Aggregates p) => Column f p
example2 = buildCol $ order [Dsc "numClasses"] $ do
  c <- refer $ known "Class"
  t <- refer $ known "Time"
  wherein $ c:."sem" :=: t:."sem"
  -- TODO perhaps move partition to above?
  partition (select ["ident"] c) [
    "ident" `indexAs` "class"
    , ccount `as` "numClasses"
    , cmin "begin" `as` "first"
    , cmax "begin" `as` "last"
    ] $ lift c
