module Data.Impl.Aggregates (
  BasicAggregates(..), Aggregates
  , index, indexAs, ccount, csum, cmax, cmin, cavg
) where

import Data.Impl.Classes ( With(..) )

data BasicAggregates =
  Index String
  | Count
  | Sum String | Avg String
  | Max String | Min String

type Aggregates = With BasicAggregates

-- |Use partition index as an aggregate.
index :: Aggregates p => String -> (String, p)
index n = indexAs n n

-- |Use partition index as an aggregate, giving different name.
indexAs :: Aggregates p => String -> String -> (String, p)
indexAs f name = (name, wrap $ Index f)

ccount :: Aggregates p => p
ccount = wrap Count

csum :: Aggregates p => String -> p
csum = wrap . Sum

cavg :: Aggregates p => String -> p
cavg = wrap . Avg

cmax :: Aggregates p => String -> p
cmax = wrap . Max

cmin :: Aggregates p => String -> p
cmin = wrap . Min

