module Data.Handle.SQLHandle where

import Data.Impl.Column ( Column(..) )
import Data.Column

data PreStmt a =
  SCond (Cond a)
  | SInt (Number Int a)
  | SFloat (Number Float a)
  | STxt (Txt a)

instance With1 Cond PreStmt where
  wrap1 = SCond
instance With1 (Number Int) PreStmt where
  wrap1 = SInt
instance With1 (Number Float) PreStmt where
  wrap1 = SFloat
instance With1 Txt PreStmt where
  wrap1 = STxt


data SelStmt f =
  Select (Tuple f)
  | From String
  | FromOn String (Single f)
  | FromSelect (SelStmt f)

data Ixed a = Ixed Int a | NoIx a

colStmt :: Column f p -> [(Int, SelStmt f)]
colStmt = undefined where
  -- Merges relevant columns
  merge [] = []
  merge (Ixed j (Known n) : NoIx (Where x) : l) = (j, FromOn n x) : merge l

  -- Extracts bindings
  extBinds (Join i c c') = (i, c) : extBinds c'
  extBinds c = [(0, c)]
