module Data.Impl.RefTuple (
  Single(..), Tuple(..), as, tuple, select, single, singular, singularTag
) where

import qualified Data.Set as S
import qualified Data.Map as M

-- |Singular data with operation f
-- Note that :. interacts poorly with TProd of TRefs. Avoid this if possible.
data Single f =
  Wrap (f (Single f))
  | (Tuple f) :. String
infix 8 :.

-- |Tuple data with operation f
data Tuple f =
  TRef Int
  | TProd [Tuple f]
  | TMap (M.Map String (Single f))

instance Semigroup (Tuple f) where
  TMap l <> TMap l' = TMap (l <> l')
  TProd l <> t = TProd (t : l)
  t <> TProd l = TProd (t : l)
  t <> t' = TProd [t, t']
instance Monoid (Tuple f) where
  mempty = TProd []

-- |Names the object, mainly as syntactic utility
as :: a -> String -> (String, a)
as = flip (,)
infix 7 `as`

-- |Creates a tuple with specified fields
tuple :: [(String, Single f)] -> Tuple f
tuple = TMap . M.fromList

-- |Selects certain fields from the tuple
select :: [String] -> Tuple f -> Tuple f
select l t = TMap $ M.fromSet (fieldOf t) fields where
  fieldOf (TMap m) n = m M.! n
  fieldOf t n = t:.n
  fields = S.fromList l

-- |Tuple with single element
single :: String -> Single f -> Tuple f
single n = TMap . M.singleton n

-- |Field tag for singular case
singularTag :: String
singularTag = ""

-- |Singular tuple
singular :: Single f -> Tuple f
singular = single singularTag