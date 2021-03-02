module Data.Named (
  module Data.Impl.Named
) where

import Data.Impl.Named (
  sym, Vals, As(..), extName
  , ListAppend, headProxy, tailProxy, ListExt, Tuple(..), (*:*)
  , TupleNormal(..), TupleElem(..), IsTuple(..)
  )
