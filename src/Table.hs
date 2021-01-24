{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Table (
  Column(..), Single(..), Tuple(..), select
  , MkCol, buildCol, refer, lift, wherein, order
  , With1(..)
  , Cond(..), Order(..), Basics(..)
  , Number(..), wrapI, wrapF, WithInt, WithFloat, Numeric
  , Txt(..), liftS, WithTxt
) where

import Control.Monad.Writer ( Writer, runWriter, tell )
import Control.Monad.State ( StateT(..), evalStateT, get, modify )
import Data.Monoid ( Endo(..) )


-- |Singular data with operation f
data Single f =
  Wrap (f (Single f))
  | (Tuple f) :. String
infix 8 :.

-- |Tuple data with operation f
data Tuple f =
  TRef Int
  | TProd (Tuple f) (Tuple f)
  | TComp [(String, Single f)] -- Perhaps Map?

instance Semigroup (Tuple f) where
  TComp l <> TComp l' = TComp (l <> l')
  t <> TComp [] = t
  TComp [] <> t = t
  t <> t' = TProd t t'
instance Monoid (Tuple f) where
  mempty = TComp []

select :: [String] -> Tuple f -> Tuple f
select l t = TComp $ zip l $ (t:.) <$> l


-- TODO Make it typed
-- TODO Insert & Remove
-- TODO Group-By & Window functions
-- TODO Disallow leaking column

data CombineOp = Union | Intersect | Diff
data RowPos = RelPos Int | MinPos | MaxPos

-- |Column data with operation f (Variable is indexed by Int)
data Column f p where
  Known :: String -> Column f p
  Lift :: Tuple f -> Column f p
  Where :: Single f -> Column f p
  Join :: Int -> Column f p -> Column f p -> Column f p

  Combine :: CombineOp -> Column f p -> Column f p -> Column f p
  Order :: Single f -> Column f p -> Column f p
  Partition :: Tuple f -> (String -> p) -> Column f p -> Column f p
  Window :: RowPos -> RowPos -> Column f p -> Column f p

newtype Final f p = Final (Column f p)

known :: String -> Final f p
known = Final . Known


type MkCol f p = StateT Int (Writer (Endo (Column f p)))

mkVar :: MkCol f p Int
mkVar = get <* modify succ

-- |Forms the column
buildCol :: MkCol f p (Column f p) -> Column f p
buildCol form = apply fin
  where (fin, Endo apply) = runWriter . (`evalStateT` 1) $ form

refer :: Column f p -> MkCol f p (Tuple f)
refer col = do
  index <- mkVar
  tell $ Endo $ Join index col
  return $ TRef index

lift :: Tuple f -> MkCol f p (Column f p)
lift s = return $ Lift s

class With p q where
  -- |Wraps form p to united form q. Use with caution, this can cause type mismatch.
  wrap :: p -> q

class With1 f g where
  -- |Wraps form f to unified form g. Use with caution, this can cause type mismatch.
  wrap1 :: f a -> g a

data Cond a =
  a :=: a | a :>: a | a :<: a
  | Cand (Cond a) (Cond a)
  | Cor (Cond a) (Cond a)
  | Cnot (Cond a) (Cond a)
infix 4 :=:
infix 4 :>:
infix 4 :<:
infixr 3 `Cand`
infixr 2 `Cor`

wherein :: (With1 Cond f) => Cond (Single f) -> MkCol f p ()
wherein cond = do
  index <- mkVar
  tell $ Endo $ Join index (Where $ Wrap . wrap1 $ cond)


data Order a =
  OrderBy a
  | Rev (Order a) | Chain (Order a) (Order a)
infixl 1 `Chain`

-- |Gives ordered column. Only the last order statement gets in effect.
order :: (With1 Order f) => Order (Single f) -> Column f p -> Column f p
order ord = Order $ Wrap . wrap1 $ ord


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


example :: (Basics f, Numeric f) => Single f -> Column f p
example ident = buildCol $ do
  c <- refer $ Known "Class"
  wherein $ c:."priority" :=: wrapI 0 `Cand` c:."course" :=: ident
  t <- refer $ Known "Time"
  wherein $ c:."sem" :=: t:."sem"
  let ord = OrderBy (c:."begin") `Chain` (Rev . OrderBy) (c:."end")
  order ord <$> lift (select ["course", "begin", "end"] c)
