module ColumnTest ( main ) where

import Data.Column

main :: IO ()
main = putStrLn "Test suite not yet implemented."


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
