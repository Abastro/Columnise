module Data.Handle.SQLHandle (
  PreStmt
) where

import Data.Impl.Column ( Column(..), ColBody(..), defWithBody )
import Data.Column

data PreStmt a =
  SBool (Cond a)
  | SInt (Number Int a)
  | SFloat (Number Float a)
  | STxt (Txt a)

instance With1 Cond PreStmt where
  wrap1 = SBool
instance With1 (Number Int) PreStmt where
  wrap1 = SInt
instance With1 (Number Float) PreStmt where
  wrap1 = SFloat
instance With1 Txt PreStmt where
  wrap1 = STxt


colStmt :: Column f p -> Column f p
colStmt = go where
  go = undefined

  expand (Join j c c') = (j, c) : expand c'
  expand c = [(0, defWithBody c)]
