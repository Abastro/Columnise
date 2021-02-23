module ColumnTest ( main ) where

import Data.Column

main :: IO ()
main = putStrLn "Test suite not yet implemented."

example1 :: (WithCond f, Numeric f) => Single f -> BuiltColumn f
example1 ident = build $ order [asc "begin", dsc "end"] . asCol $ do
  t <- refer $ known "Time"
  c <- refer $ known "Class"
  wherein $ c:."priority" :>=: wrapI 0 `Cand` c:."course" :=: ident
  wherein $ c:."sem" :=: t:."sem"
  lift $ select ["course", "begin", "end"] c

example2 :: (WithCond f, Aggregates f) => BuiltColumn f
example2 = build $ order [dsc "numClasses"]
  . partitions ["ident"] [
    "ident" `indexAs` "class"
    , ccount `as` "numClasses"
    , cmin "begin" `as` "first"
    , cmax "begin" `as` "last"
  ] . asCol $ do
    t <- refer $ known "Time"
    c <- refer $ known "Class"
    wherein $ c:."sem" :=: t:."sem"
    lift c
