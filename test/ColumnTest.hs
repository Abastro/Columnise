module ColumnTest ( main ) where

import Data.Column

main :: IO ()
main = putStrLn "Test suite not yet implemented."

example1 :: (WithCond f, Numeric f) => Single f -> BuiltColumn f p
example1 ident = build $ order [asc "begin", dsc "end"] $ do
  t <- refer $ known "Time"
  c <- refer $ known "Class"
  wherein $ c:."priority" :>=: wrapI 0 `Cand` c:."course" :=: ident
  wherein $ c:."sem" :=: t:."sem"
  lift $ select ["course", "begin", "end"] c

example2 :: (WithCond f, Aggregates p) => BuiltColumn f p
example2 = build $ order [dsc "numClasses"]
  . partitions [part "ident"] [
    "ident" `indexAs` "class"
    , ccount `as` "numClasses"
    , cmin "begin" `as` "first"
    , cmax "begin" `as` "last"
  ] $ do
    t <- refer $ known "Time"
    c <- refer $ known "Class"
    wherein $ c:."sem" :=: t:."sem"
    lift c
