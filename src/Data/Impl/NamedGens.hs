{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Impl.NamedGens where

import Data.Impl.Named
import GHC.TypeLits
import GHC.Generics

-- TODO This could be bad, general solution for sum & product required
{-
type family AsTuple t :: [*] where
  -- Simply passes the metadata
  AsTuple (M1 _ (MetaData _ _ _ _) f) = AsTuple f
  -- Captures {.. , foo :: c ..}
  AsTuple (M1 _ (MetaSel (Just n) _ _ _) (K1 _ c)) = '[As n  c]
  -- Appends the two
  AsTuple (f :*: g) = ListAppend (AsTuple f) (AsTuple g)
  AsTuple _ = TypeError (Text "Can't derive tuple, it needs to be a symple record")

class Generic t => GIsTuple t where
  gTuple :: t -> Tuple (AsTuple (Rep t))
-}
--instance GIsTuple t => IsTuple t where
--  type Elems t = AsTuple (Rep t)
