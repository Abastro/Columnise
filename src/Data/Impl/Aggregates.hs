module Data.Impl.Aggregates (
  BasicAggregates(..), Aggregates
  , index, indexAs, ccount, csum, cmax, cmin, cavg
) where

import Data.Impl.Classes ( Field(..), WithA(..) )

data BasicAggregates =
  Index String
  | Count
  | Sum String | Avg String
  | Max String | Min String

type Aggregates = WithA BasicAggregates

-- |Use partition index as an aggregate.
index :: Aggregates f => String -> (String, Aggregate f)
index n = indexAs n n

-- |Use partition index as an aggregate, giving different name.
indexAs :: Aggregates f => String -> String -> (String, Aggregate f)
indexAs f name = (name, wrapA $ Index f)

ccount :: Aggregates f => Aggregate f
ccount = wrapA Count

csum :: Aggregates f => String -> Aggregate f
csum = wrapA . Sum

cavg :: Aggregates f => String -> Aggregate f
cavg = wrapA . Avg

cmax :: Aggregates f => String -> Aggregate f
cmax = wrapA . Max

cmin :: Aggregates f => String -> Aggregate f
cmin = wrapA . Min

