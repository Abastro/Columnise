{-# LANGUAGE MultiParamTypeClasses #-}
module SQLHandle where

import Table

data PreStmt a =
  SCond (Cond a)
  | SOrd (Order a)
  | SInt (Number Int a)
  | SFloat (Number Float a)
  | STxt (Txt a)

instance With1 Cond PreStmt where
  wrap1 = SCond
instance With1 Order PreStmt where
  wrap1 = SOrd
instance Basics PreStmt

instance With1 (Number Int) PreStmt where
  wrap1 = SInt
instance With1 (Number Float) PreStmt where
  wrap1 = SFloat
instance Numeric PreStmt

instance With1 Txt PreStmt where
  wrap1 = STxt


data SelStmt f =
  Select (Tuple f)
  | From String
  | FromOn String (Single f)

data Ixed a = Ixed Int a | NoIx a

colStmt :: Column f p -> [(Int, Column f p)]
colStmt = undefined where
  merge (Ixed j (Known n) : NoIx (Where x) : l) = (j, FromOn n x) : merge l

  extCol (Join i c c') = Ixed i c : extCol c'
  extCol c@(Known _) = [Ixed 0 c]
  extCol c = [NoIx c]
