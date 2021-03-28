module ColumnTest ( main ) where

import Data.Column

main :: IO ()
main = putStrLn "Test suite not yet implemented."

example1 :: Single f -> Column f
example1 ident =
  withTr (sorts $ \t -> [t:."begin" `as` Asc, t:."end" `as` Dsc])
  $ asCol $ do
  t <- refer $ known "Time"
  c <- refer $ known "Class"
  wherein $ c:."priority" :>=: wrapI 0 `Cand` c:."course" :=: ident
  wherein $ c:."sem" :=: t:."sem"
  pure $ select ["course", "begin", "end"] c

example2 :: Column f
example2 =
  withTr (sorts $ \t -> [t:."numClasses" `as` Dsc])
  $ groups (select ["ident"]) (\l -> fmap tuple . traverse sequenceA $ [
      (:."ident") <$> origin `as` "class"
      , collapse (ccount <$ l) `as` "numClasses"
      , collapse (cmin . (:."begin") <$> l) `as` "first"
      , collapse (cmax . (:."begin") <$> l) `as` "last"])
  $ asCol $ do
    t <- refer $ known "Time"
    c <- refer $ known "Class"
    wherein $ c:."sem" :=: t:."sem"
    pure c
