{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Impl.Named (
  sym, Operable(..), Vals(..), As(..), extName
  , ListAppend, headProxy, tailProxy, ListExt, Tuple(..), (*:*)
  , TupleNormal(..), TupleElem(..), IsTuple(..)
) where

import Data.Proxy
import GHC.TypeLits

sym :: forall (s :: Symbol). Proxy s
sym = Proxy

type family ListAppend l l' where
  ListAppend '[] l' = l'
  ListAppend (a ': l) l' = a ': ListAppend l l'

headProxy :: Proxy (a ': l) -> Proxy a
headProxy _ = Proxy

tailProxy :: Proxy (a ': l) -> Proxy l
tailProxy _ = Proxy

data Operable f =
  Refer Int String -- With tuple reference and its element
  | Operate (f (Operable f))
data Vals f a =
  -- Creates from external value
  FromVal a
  -- With tuple reference and its element
  | Operable (Operable f)
newtype As (s :: Symbol) f a = As (Vals f a)

extName :: KnownSymbol s => Proxy (As s f a) -> String
extName = symbolVal . convProxy where
  convProxy :: Proxy (As s f a) -> Proxy s
  convProxy _ = Proxy

type family ListExt s l where
  ListExt s (As s f a ': l) = Vals f a
  ListExt s (_ ': l) = ListExt s l

data Tuple (l :: [*]) where
  TEmpty :: Tuple '[]
  (:::) :: As s f a -> Tuple l -> Tuple (As s f a ': l)
infixr 7 :::

(*:*) :: Tuple l -> Tuple l' -> Tuple (ListAppend l l')
TEmpty *:* t' = t'
(a ::: t) *:* t' = a ::: (t *:* t')
infixr 1 *:*

class TupleNormal l where
  referred :: Int -> Proxy l -> Tuple l
instance TupleNormal '[] where
  referred _ _ = TEmpty
instance (KnownSymbol s, TupleNormal l) => TupleNormal (As s f a ': l) where
  referred j p = As (Operable . Refer j . extName $ headProxy p) ::: referred j (tailProxy p)

class TupleElem s l where
  (.:) :: Tuple l -> Proxy s -> ListExt s l
infix 8 .:
instance {-# OVERLAPS #-} TupleElem s (As s f a ': l) where
  (As v ::: _) .: _ = v
instance (ListExt s (b ': l) ~ ListExt s l, TupleElem s l) => TupleElem s (b ': l) where
  (_ ::: t) .: p = t .: p

class IsTuple t where
  type Elems t :: [*]
  tuple :: t -> Tuple (Elems t)
  fromTuple :: Tuple (Elems t) -> t
instance IsTuple (As s f a) where
  type Elems (As s f a) = '[As s f a]
  tuple t = t ::: TEmpty
  fromTuple (t ::: TEmpty) = t
instance IsTuple (As s f a, As s' f' a') where
  type Elems (As s f a, As s' f' a') = '[As s f a, As s' f' a']
  tuple (t, t') = t ::: t' ::: TEmpty
  fromTuple (t ::: t' ::: TEmpty) = (t, t')
instance IsTuple (As s f a, As s' f' a', As s'' f'' a'') where
  type Elems (As s f a, As s' f' a', As s'' f'' a'') =
    '[As s f a, As s' f' a', As s'' f'' a'']
  tuple (t, t', t'') = t ::: t' ::: t'' ::: TEmpty
  fromTuple (t ::: t' ::: t'' ::: TEmpty) = (t, t', t'')
-- I'm dead
instance IsTuple (As s f a, As s' f' a', As s'' f'' a'', As s''' f''' a''') where
  type Elems (As s f a, As s' f' a', As s'' f'' a'', As s''' f''' a''') =
    '[As s f a, As s' f' a', As s'' f'' a'', As s''' f''' a''']
  tuple (t, t', t'', t''') = t ::: t' ::: t'' ::: t''' ::: TEmpty
  fromTuple (t ::: t' ::: t'' ::: t''' ::: TEmpty) = (t, t', t'', t''')
instance IsTuple (As s f a, As s' f' a', As s'' f'' a'', As s''' f''' a''', As s'''' f'''' a'''') where
  type Elems (As s f a, As s' f' a', As s'' f'' a'', As s''' f''' a''', As s'''' f'''' a'''') =
    '[As s f a, As s' f' a', As s'' f'' a'', As s''' f''' a''', As s'''' f'''' a'''']
  tuple (t, t', t'', t''', t'''') = t ::: t' ::: t'' ::: t''' ::: t'''' ::: TEmpty
  fromTuple (t ::: t' ::: t'' ::: t''' ::: t'''' ::: TEmpty) = (t, t', t'', t''', t'''')
-- TODO Generically handle this?


exTup1 :: Tuple '[As "noh" f Int, As "meh" f Float]
exTup1 = tuple (As @"noh" $ FromVal 3, As @"meh" $ FromVal 1.0)

exTup2 :: Tuple '[As "pl" f Double, As "kek" f (Maybe Int), As "dream" f Int]
exTup2 = referred 1 (Proxy @'[_, _]) *:* tuple (As @"dream" $ FromVal 3)

exTup3 :: Tuple '[As "noh" f Int, As "meh" f Float, As "pl" f Double, As "kek" f (Maybe Int), As "dream" f Int]
exTup3 = exTup1 *:* exTup2

exExt :: Vals f Int
exExt = exTup3 .:sym @"noh"
