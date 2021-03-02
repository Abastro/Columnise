module Data.Impl.Aggregates (
  BasicAggregates(..), Aggregates, elemsOf
  , ccount, csum, cmax, cmin, cavg
) where

import Data.Impl.Classes ( Field(..), WithA(..) )

data BasicAggregates a =
  Count
  | Sum a | Avg a
  | Max a | Min a

type Aggregates = WithA BasicAggregates

-- |Impl-only function for retrieving elements
elemsOf :: BasicAggregates a -> [a]
elemsOf Count = []
elemsOf (Sum s) = [s]
elemsOf (Avg s) = [s]
elemsOf (Max s) = [s]
elemsOf (Min s) = [s]

ccount :: Aggregates f => Aggregate f a
ccount = wrapA Count

csum :: Aggregates f => a -> Aggregate f a
csum = wrapA . Sum

cavg :: Aggregates f => a -> Aggregate f a
cavg = wrapA . Avg

cmax :: Aggregates f => a -> Aggregate f a
cmax = wrapA . Max

cmin :: Aggregates f => a -> Aggregate f a
cmin = wrapA . Min

