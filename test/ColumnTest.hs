module ColumnTest ( main ) where

import Data.Column

main :: IO ()
main = putStrLn "Test suite not yet implemented."

example1 :: (WithCond f, Numeric f) => Single f -> Column f p
example1 ident = buildCol $ order [Asc "begin", Dsc "end"] $ do
  t <- refer $ known "Time"
  c <- refer $ known "Class"
  wherein $ c:."priority" :>=: wrapI 0 `Cand` c:."course" :=: ident
  wherein $ c:."sem" :=: t:."sem"
  lift $ select ["course", "begin", "end"] c

example2 :: (WithCond f, Aggregates p) => Column f p
example2 = buildCol $ order [Dsc "numClasses"]
  . partition [Keep "ident"] [
    "ident" `indexAs` "class"
    , ccount `as` "numClasses"
    , cmin "begin" `as` "first"
    , cmax "begin" `as` "last"
  ] $ do
    t <- refer $ known "Time"
    c <- refer $ known "Class"
    wherein $ c:."sem" :=: t:."sem"
    lift c
