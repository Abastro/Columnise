module Data.RefTuple (
  Single(..), Tuple(..), select, singular, singularTag
) where

-- |Singular data with operation f
data Single f =
  Wrap (f (Single f))
  | (Tuple f) :. String
infix 8 :.

-- |Tuple data with operation f
data Tuple f =
  TRef Int
  | TProd (Tuple f) (Tuple f)
  | Tuple [(String, Single f)] -- TODO Perhaps Map?

instance Semigroup (Tuple f) where
  Tuple l <> Tuple l' = Tuple (l <> l')
  t <> Tuple [] = t
  Tuple [] <> t = t
  t <> t' = TProd t t'
instance Monoid (Tuple f) where
  mempty = Tuple []

-- |Selects certain fields from the tuple
select :: [String] -> Tuple f -> Tuple f
select l t = Tuple $ zip l $ (t:.) <$> l

-- |Field tag for singular case
singularTag :: String
singularTag = ""

-- |Singular tuple
singular :: Single f -> Tuple f
singular s = Tuple [(singularTag, s)]
