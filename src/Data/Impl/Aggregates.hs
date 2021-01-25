module Data.Impl.Aggregates (
  BasicAggregates(..), BasicAggs
  , index, ccount, csum, cmax, cmin, cavg
) where

import Data.Impl.Classes ( With(..) )

data BasicAggregates =
  Index String
  | Count
  | Sum String | Avg String
  | Max String | Min String

type BasicAggs = With BasicAggregates

-- |Use partition index as an aggregate.
index :: BasicAggs p => String -> (String, p)
index n = (n, wrap $ Index n)

ccount :: BasicAggs p => p
ccount = wrap Count

csum :: BasicAggs p => String -> p
csum = wrap . Sum

cavg :: BasicAggs p => String -> p
cavg = wrap . Avg

cmax :: BasicAggs p => String -> p
cmax = wrap . Max

cmin :: BasicAggs p => String -> p
cmin = wrap . Min

