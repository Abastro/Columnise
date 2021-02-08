module Data.RefTuple (
  Single(..), Tuple(..), as, select, single, singular, singularTag
) where

import qualified Data.Set as S
import qualified Data.Map as M

-- |Singular data with operation f
data Single f =
  Wrap (f (Single f))
  | (Tuple f) :. String
infix 8 :.

-- |Tuple data with operation f
data Tuple f =
  TRef Int
  | TProd [Tuple f]
  | Tuple (M.Map String (Single f))

instance Semigroup (Tuple f) where
  Tuple l <> Tuple l' = Tuple (l <> l')
  TProd l <> t = TProd (t : l)
  t <> TProd l = TProd (t : l)
  t <> t' = TProd [t, t']
instance Monoid (Tuple f) where
  mempty = TProd []

-- |Names the object, mainly as syntactic utility
as :: a -> String -> (String, a)
as = flip (,)
infix 7 `as`

-- |Selects certain fields from the tuple
select :: [String] -> Tuple f -> Tuple f
select l t = Tuple $ M.fromSet (fieldOf t) fields where
  fieldOf (Tuple m) n = m M.! n
  fieldOf t n = t:.n
  fields = S.fromList l

-- |Tuple with single element
single :: String -> Single f -> Tuple f
single n = Tuple . M.singleton n

-- |Field tag for singular case
singularTag :: String
singularTag = ""

-- |Singular tuple
singular :: Single f -> Tuple f
singular = single singularTag
