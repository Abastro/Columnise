{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Impl.Named (
  sym, syms, Operable(..), Vals(..), As(..), wraps, extName
  , ListMap, ListAppend, headProxy, tailProxy, ListExt, Tuple(..), (*:*)
  , seqTuple, TupRaw, TupleNormal(..), (.:), IsTuple(..)
) where

import Data.Proxy
import GHC.TypeLits
import Data.Impl.Operable

sym :: forall (s :: Symbol). Proxy s
sym = Proxy
syms :: forall (ls :: [Symbol]). Proxy ls
syms = Proxy

-- Denotes application over list
type family ListMap m l where
  ListMap m '[] = '[]
  ListMap m (a ': l) = m a ': ListMap m l
-- Denotes append over list
type family ListAppend l l' where
  ListAppend '[] l' = l'
  ListAppend (a ': l) l' = a ': ListAppend l l'

headProxy :: Proxy (a ': l) -> Proxy a
headProxy _ = Proxy

tailProxy :: Proxy (a ': l) -> Proxy l
tailProxy _ = Proxy

newtype Vals f (a :: *) = Vals { getVals :: Operable f }
-- TODO Perhaps symbol as separate?
newtype As (s :: Symbol) f a = As { getAsIn :: Vals f a }

-- |Wraps into value
wraps :: Wraps a f => a -> Vals f a
wraps = Vals . Operate . wrapInto

extName :: KnownSymbol s => Proxy (As s f a) -> String
extName = symbolVal . convProxy where
  convProxy :: Proxy (As s f a) -> Proxy s
  convProxy _ = Proxy

-- Extracts field from a tuple list
type family ListExt s l where
  ListExt s (As s f a ': l) = Vals f a
  ListExt s (_ ': l) = ListExt s l
type family ListExts ls l where
  ListExts '[] '[] = '[]
  ListExts (s ': ls) (As s f a ': l) = As s f a ': ListExts ls l
  ListExts ls (_ ': l) = ListExts ls l

-- |Generalized datatype for tuple (normal tuples are hard to generalize)
data Tuple (l :: [*]) where
  TEmpty :: Tuple '[]
  (:::) :: a -> Tuple l -> Tuple (a ': l)
infixr 7 :::

(*:*) :: Tuple l -> Tuple l' -> Tuple (ListAppend l l')
TEmpty *:* t' = t'
(a ::: t) *:* t' = a ::: (t *:* t')
infixr 1 *:*

class TupleElem s l where
  (.:) :: Tuple l -> Proxy s -> ListExt s l
  infix 8 .:
instance {-# OVERLAPS #-} TupleElem s (As s f a ': l) where
  (As v ::: _) .: _ = v
instance (ListExt s (b ': l) ~ ListExt s l, TupleElem s l) => TupleElem s (b ': l) where
  (_ ::: t) .: p = t .: p

-- TODO Order-independent
class TupleProj ls l where
  project :: Proxy ls -> Tuple l -> Tuple (ListExts ls l)
instance TupleProj '[] '[] where
  project _ _ = TEmpty
instance {-# OVERLAPS #-} TupleProj ls l => TupleProj (s ': ls) (As s f a ': l) where
  project p (v ::: m) = v ::: project (tailProxy p) m
instance (ListExts ls (b ': l) ~ ListExts ls l, TupleProj ls l) => TupleProj ls (b ': l) where
  project p (_ ::: t) = project p t

class TupleSeq l where
  -- |Sequences tuple through the applicative
  seqTuple :: Applicative m => Tuple (ListMap m l) -> m (Tuple l)
instance TupleSeq '[] where
  seqTuple TEmpty = pure TEmpty
instance TupleSeq l => TupleSeq (a ': l) where
  seqTuple (x ::: xs) = (:::) <$> x <*> seqTuple xs

tupLProxy :: Proxy (Tuple l) -> Proxy l
tupLProxy _ = Proxy

type TupRaw f = [(String, Operable f)]

class TupleNormIn f l where
  fieldsIn :: Proxy l -> Proxy f -> [String]
  asRawIn :: Tuple l -> TupRaw f
  referredIn :: Int -> Proxy f -> Tuple l
instance TupleNormIn f '[] where
  fieldsIn _ _ = []
  asRawIn _ = []
  referredIn _ _ = TEmpty
instance (KnownSymbol s, TupleNormIn f l) => TupleNormIn f (As s f a ': l) where
  fieldsIn pl pf = extName (headProxy pl) : fieldsIn (tailProxy pl) pf
  asRawIn (x ::: t) = (extName $ pure x, getVals $ getAsIn x) : asRawIn t
  referredIn j pf = t where
    x = As (Vals . Refer j . extName $ pure x)
    t = x ::: referredIn j pf

class TupleNormIn f l => TupleNormal l f | l -> f where
  proxyF :: Proxy (Tuple l) -> Proxy f
  proxyF _ = Proxy
  -- |Get fields
  fields :: Proxy (Tuple l) -> [String]
  fields p = fieldsIn (tupLProxy p) (proxyF p)
  asRaw :: Tuple l -> [(String, Operable f)]
  asRaw = asRawIn
  -- |Get referred tuple
  referred :: Int -> Tuple l
  referred j = t where t = referredIn j (proxyF $ pure t)
instance {-# OVERLAPS #-} KnownSymbol s => TupleNormal '[As s f a] f
instance (KnownSymbol s, TupleNormal l f) => TupleNormal (As s f a ': l) f

class IsTuple t where
  type Elems t :: [*]
  tuple :: t -> Tuple (Elems t)
  fromTuple :: Tuple (Elems t) -> t
instance IsTuple (As s f a) where
  type Elems (As s f a) = '[As s f a]
  tuple t = t ::: TEmpty
  fromTuple (t ::: TEmpty) = t
instance IsTuple (a, a') where
  type Elems (a, a') = '[a, a']
  tuple (t, t') = t ::: t' ::: TEmpty
  fromTuple (t ::: t' ::: TEmpty) = (t, t')
instance IsTuple (a, a', a'') where
  type Elems (a, a', a'') = '[a, a', a'']
  tuple (t, t', t'') = t ::: t' ::: t'' ::: TEmpty
  fromTuple (t ::: t' ::: t'' ::: TEmpty) = (t, t', t'')
-- I'm dead
instance IsTuple (a, a', a'', a''') where
  type Elems (a, a', a'', a''') = '[a, a', a'', a''']
  tuple (t, t', t'', t''') = t ::: t' ::: t'' ::: t''' ::: TEmpty
  fromTuple (t ::: t' ::: t'' ::: t''' ::: TEmpty) = (t, t', t'', t''')
instance IsTuple (a, a', a'', a''', a'''') where
  type Elems (a, a', a'', a''', a'''') = '[a, a', a'', a''', a'''']
  tuple (t, t', t'', t''', t'''') = t ::: t' ::: t'' ::: t''' ::: t'''' ::: TEmpty
  fromTuple (t ::: t' ::: t'' ::: t''' ::: t'''' ::: TEmpty) = (t, t', t'', t''', t'''')
-- MAYBE Generically handle this?

exTup1 :: BasicWraps f => Tuple '[As "noh" f Int, As "meh" f Float]
exTup1 = tuple (As @"noh" $ wraps 3, As @"meh" $ wraps 1.0)

exTup2 :: BasicWraps f => Tuple '[As "pl" f Double, As "kek" f (Maybe Int), As "dream" f Int]
exTup2 = referred @'[_, _] 1 *:* tuple (As @"dream" $ wraps 3)

exTup3 :: BasicWraps f => Tuple '[As "noh" f Int, As "meh" f Float, As "pl" f Double, As "kek" f (Maybe Int), As "dream" f Int]
exTup3 = exTup1 *:* exTup2

exExt :: BasicWraps f => Vals f Int
exExt = exTup3 .:sym @"noh"

exProj :: BasicWraps f => Tuple '[As "noh" f Int, As "pl" f Double]
exProj = project (syms @'["noh", "pl"]) exTup3
