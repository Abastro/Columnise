{-# LANGUAGE PatternSynonyms #-}
module Data.Impl.Singles where

-- TODO Better export

import Data.Impl.RefTuple ( Single(..) )
import Data.Impl.Utils
import Data.Impl.Column ( ColBody(..), JoinCol, joining )
import Data.Impl.Classes

-- TODO Conventionally, boolean is stored as a small number.
-- Might better represent that part.
data Cond a =
  AsCond !a
  | a :=: a | a :>: a | a :<: a
  | Cand (Cond a) (Cond a)
  | Cor (Cond a) (Cond a)
  | Cnot (Cond a)
  deriving Functor
infix 4 :=:
infix 4 :>:
infix 4 :<:
infix 4 :>=:
infix 4 :<=:
infixr 3 `Cand`
infixr 2 `Cor`

pattern (:>=:) :: a -> a -> Cond a
pattern x :>=: y = Cnot (x :<: y)

pattern (:<=:) :: a -> a -> Cond a
pattern x :<=: y = Cnot (x :>: y)

type WithCond = WithF Cond

wrapC :: WithF Cond f => Cond (Single f) -> Single f
wrapC = Wrap . wrap

wherein :: (WithF Cond f) => Cond (Single f) -> JoinCol f ()
wherein cond = () <$ joining (fromBody . pure . Where $ wrapC cond)


data Number n a =
  AsNum !a   -- For slightly better safety, adds extra step for conversion
  | PrimNum !n
  | Add (Number n a) (Number n a)
  | Mult (Number n a) (Number n a)
  | Div (Number n a) (Number n a)
  | Negate (Number n a)
  deriving Functor

-- Inheriting typeclass only for overloading
instance Num n => Num (Number n a) where
  (+) = Add
  (*) = Mult
  negate = Negate
  fromInteger = PrimNum . fromInteger
  abs = undefined; signum = undefined

-- Inheriting typeclass only for overloading
instance Fractional n => Fractional (Number n a) where
  (/) = Div
  fromRational = PrimNum . fromRational

-- TODO Add for other primitive types
-- |Wraps an integer into the field
wrapI :: WithF (Number Int) f => Number Int (Single f) -> Single f
wrapI = Wrap . wrap

-- |Wraps a float into the field
wrapF :: WithF (Number Float) f => Number Float (Single f) -> Single f
wrapF = Wrap . wrap

type WithInt = WithF (Number Int)
type WithFloat = WithF (Number Float)
type Numeric f = (WithInt f, WithFloat f)


data Txt a =
  AsTxt !a
  | PrimTxt !String
  | TAppend (Txt a) (Txt a)
  deriving Functor

instance Semigroup (Txt a) where
  (<>) = TAppend

type WithTxt = WithF Txt

liftS :: WithTxt f => String -> f a
liftS = wrap . PrimTxt
