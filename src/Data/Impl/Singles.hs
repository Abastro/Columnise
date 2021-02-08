{-# LANGUAGE PatternSynonyms #-}
module Data.Impl.Singles where

-- TODO Better export

import Data.RefTuple ( Single(..) )
import Data.Impl.Column (
  ColBody(..), defWithBody, MkCol, joining
  )
import Data.Impl.Classes ( With1(..) )

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

type WithCond = With1 Cond

wrapC :: With1 Cond f => Cond (Single f) -> Single f
wrapC = Wrap . wrap1

wherein :: (With1 Cond f) => Cond (Single f) -> MkCol f p ()
wherein cond = () <$ joining (defWithBody . Where $ wrapC cond)


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
wrapI :: With1 (Number Int) f => Number Int (Single f) -> Single f
wrapI = Wrap . wrap1

-- |Wraps a float into the field
wrapF :: With1 (Number Float) f => Number Float (Single f) -> Single f
wrapF = Wrap . wrap1

type WithInt = With1 (Number Int)
type WithFloat = With1 (Number Float)
type Numeric f = (WithInt f, WithFloat f)


data Txt a =
  AsTxt !a
  | PrimTxt !String
  | TAppend (Txt a) (Txt a)
  deriving Functor

instance Semigroup (Txt a) where
  (<>) = TAppend

type WithTxt = With1 Txt

liftS :: WithTxt f => String -> f a
liftS = wrap1 . PrimTxt
