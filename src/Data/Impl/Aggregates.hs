module Data.Impl.Aggregates (
  BasicAggregates(..), Aggregates, elemsOf
  , index, indexAs, ccount, csum, cmax, cmin, cavg
) where

import Data.Impl.Classes ( Field(..), WithA(..) )

data BasicAggregates a =
  Index a
  | Count
  | Sum a | Avg a
  | Max a | Min a

type Aggregates = WithA BasicAggregates

-- |Impl-only function for retrieving elements
elemsOf :: BasicAggregates a -> [a]
elemsOf (Index s) = [s]
elemsOf Count = []
elemsOf (Sum s) = [s]
elemsOf (Avg s) = [s]
elemsOf (Max s) = [s]
elemsOf (Min s) = [s]

-- |Use partition index as an aggregate.
index :: Aggregates f => String -> (String, Aggregate f String)
index n = indexAs n n

-- |Use partition index as an aggregate, giving different name.
indexAs :: Aggregates f => String -> String -> (String, Aggregate f String)
indexAs f name = (name, wrapA $ Index f)

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

