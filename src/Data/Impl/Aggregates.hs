module Data.Impl.Aggregates (
  BasicAggregates(..), BasicAggs
  , ccount, csum, cmax, cmin, cavg
) where

import Data.Impl.Classes ( With(..) )

data BasicAggregates =
  Count | Sum | Max | Min | Avg

type BasicAggs = With BasicAggregates

ccount :: BasicAggs p => p
ccount = wrap Count

csum :: BasicAggs p => p
csum = wrap Sum

cmax :: BasicAggs p => p
cmax = wrap Max

cmin :: BasicAggs p => p
cmin = wrap Min

cavg :: BasicAggs p => p
cavg = wrap Avg
