{-# LANGUAGE PatternSynonyms #-}
module Data.Impl.Singles where

import Control.Monad.Writer ( tell )
import Data.Monoid ( Endo(..) )

import Data.RefTuple ( Single(..) )
import Data.Impl.Column (
  Column(..) , MkCol, Build, liftBuild, mkVar
  )
import Data.Impl.Classes ( With1(..) )

data Cond a =
  a :=: a | a :>: a | a :<: a
  | Cand (Cond a) (Cond a)
  | Cor (Cond a) (Cond a)
  | Cnot (Cond a)
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


wherein :: (With1 Cond f) => Cond (Single f) -> MkCol f p ()
wherein cond = do
  index <- mkVar
  tell $ Endo $ Join index (Where $ Wrap . wrap1 $ cond)


data Order a =
  OrderBy a
  | Rev (Order a) | Chain (Order a) (Order a)
infixl 1 `Chain`

-- |Gives ordered column. Only the last order statement gets in effect.
order :: (With1 Order f) => Order (Single f) -> Build f p -> Build f p
order ord = liftBuild (Order $ Wrap . wrap1 $ ord)

-- |Basic operations which are most likely supported
class (With1 Cond f, With1 Order f) => Basics f where


data Number n a =
  AsNum !a   -- For slightly better safety, adds extra step for conversion
  | PrimNum n
  | Add (Number n a) (Number n a)
  | Mult (Number n a) (Number n a)
  | Div (Number n a) (Number n a)
  | Negate (Number n a)

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
wrapI :: With1 (Number Int) f => Number Int (Single f) -> Single f
wrapI = Wrap . wrap1

wrapF :: With1 (Number Float) f => Number Float (Single f) -> Single f
wrapF = Wrap . wrap1

type WithInt = With1 (Number Int)
type WithFloat = With1 (Number Float)
class (WithInt f, WithFloat f) => Numeric f where


data Txt a =
  AsTxt !a
  | PrimTxt String
  | TAppend (Txt a) (Txt a)

instance Semigroup (Txt a) where
  (<>) = TAppend

type WithTxt = With1 Txt

liftS :: WithTxt f => String -> f a
liftS = wrap1 . PrimTxt
