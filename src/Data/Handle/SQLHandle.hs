{-# LANGUAGE GADTSyntax #-}
module Data.Handle.SQLHandle (
  SingleStmt, SelJoin(..), SelCore(..), SelStmt
  , Medium(..), SQLFormat(..), SQLFormer(..), formSQL, defFormer
) where

import Data.Foldable
import Data.List
import Data.Maybe
import Data.String ( IsString(..) )
import qualified Data.Map as M

import Data.Impl.Column
import Data.Column

-- TODO SingleStmt does not look like it belongs here
data SingleStmt a =
  SBool (Cond a)
  | SInt (Number Int a)
  | SFloat (Number Float a)
  | STxt (Txt a)
  deriving Functor

instance With1 Cond SingleStmt where
  wrap1 = SBool
instance With1 (Number Int) SingleStmt where
  wrap1 = SInt
instance With1 (Number Float) SingleStmt where
  wrap1 = SFloat
instance With1 Txt SingleStmt where
  wrap1 = STxt


data SelJoin f p = SelJoin {
  joinVar :: Int
  , joinSrc :: Either String (SelStmt f p)
  , joinOn :: Maybe (Single f)
}

selJoin :: Int -> Either String (SelStmt f p) -> SelJoin f p
selJoin j s = SelJoin j s Nothing

data SelCore f p where
  Values :: [Tuple f] -> SelCore f p
  Selects :: Tuple f -> [SelJoin f p] -> SelCore f p
  Combines :: CombineOp -> SelCore f p -> SelCore f p -> SelCore f p

data SProps f p a = SProps {
  sBody :: a
  , sOrd :: [(OrdDir, Single f)]
  , sPart :: Maybe ([Single f], [(p, Single f)])
  , sWindow :: Maybe (RowPos, RowPos)
} deriving (Functor, Foldable, Traversable)

type SelStmt f p = ColProps p (SelCore f p)

defSel :: SelStmt f p
defSel = Selects mempty [] <$ defColumn

procStmt :: (With1 Cond f, Functor f) => Column f p -> Fresh (SelStmt f p)
procStmt = traverse (fmap catStmts . simplify) where
  simplify body = case body of
    -- Non-join statements
    Known n -> do k <- fresh; pure $ Selects (TRef k) [selJoin k $ Left n]
    Lift t -> pure $ Selects t []
    Where _ -> error "Where statement cannot be the last"
    Combine m c c' -> Combines m <$> simplify c <*> simplify c'

    -- Partitioned column cannot be simplified
    -- TODO In fact, window require simplifications; todo for later
    Join j c@CProps{ colPart = Just _ } b -> do
      stmt <- Right <$> procStmt c
      simplify b >>= joining (selJoin j stmt)
    -- Order without partition is discarded with single statement joins

    -- Known-Where: groups the two into join
    Join j CProps{ colBody = Known n } (Join _ CProps{ colBody = Where s } b) ->
      simplify b >>= joining (selJoin j $ Left n){ joinOn = Just s }
    -- Known to From
    Join j CProps{ colBody = Known n } b ->
      simplify b >>= joining (selJoin j $ Left n)
    -- Joining Lift simply leads to substitutions of latter statements
    Join j CProps{ colBody = Lift t } b ->
      simplify (subsBody j t b)
    -- Gathers where statement for use
    Join _ CProps { colBody = Where s } b ->
      simplify b >>= joining (selJoin whBind $ Left ""){ joinOn = Just s }
    -- Using associativity to embed Join when ordering is not specified
    Join j CProps{ colBody = Join k ci bi, colOrd = [] } b ->
      simplify $ Join k ci $ Join j (defWithBody bi) b
    -- Otherwise, do not simplify
    Join j c b -> do
      stmt <- Right <$> procStmt c
      simplify b >>= joining (selJoin j stmt)

  whBind = 0
  -- Concats where statements(into singular where) and singular tuples(into values)
  -- Also rearrange associative operations to be infix-left.
  catStmts sel = case sel of
    Selects t [] -> Values [t] -- When only values exist
    Selects t js -> Selects t after where
      (wheres, others) = partition ((== whBind) . joinVar) js
      whStmt = wrapC . foldl1 Cand . map AsCond
      after = case (joinOn <$> wheres, others) of
        ([], _) -> others
        (_, []) -> error "Where statements can't be the only ones"
        (whs, fs : os) ->
          fs{ joinOn = Just . whStmt $ catMaybes (joinOn fs : whs) } : os
    -- Right first to Left first
    Combines m c (Combines m' c' c'') | m == m' && m /= Diff ->
      catStmts $ Combines m (Combines m c c') c''
    -- Recursive application for Combine, as simplify itself does not apply to all
    Combines m c c' -> let cs = catStmts c; cs' = catStmts c'
      in case (m, cs, cs') of
        (Union, Values l, Values l') -> Values $ l <> l' -- Unions two values
        _ -> Combines m cs cs'
    Values _ -> sel -- Nothing to concat

  -- TODO Partitioned Join >> Where >> Lift => Having
  -- TODO Processing omits in partition and order
  -- Both requires change in SelStmt format; Notably, Props
  procOmit c@CProps{ colBody = Selects t js, colOrd = ord, colPart = part } = undefined
    where
      omit = (snd . ins <$> filter omits ord)
        <> (ins <$> filter omits (fold $ fst <$> part))
  procOmit _ = undefined

  -- Joining join with existing core
  joining join core = case core of
    Selects t js -> pure . Selects t $ join : js
    _ -> do
      k <- fresh; pure $ Selects (TRef k) [join, selJoin k . Right $ core <$ defSel]

  -- Substitutions; Replace binding j with tuple t.
  subsBody j t body = case body of
    Known n -> Known n
    Lift tup -> Lift $ subsTuple j t tup
    Where s -> Where $ subsSingle j t s
    Join k c b -> Join k (subsBody j t <$> c) $ subsBody j t b
    Combine m c c' -> Combine m (subsBody j t c) (subsBody j t c')

  subsTuple j t tup = case tup of
    TRef k -> if j == k then t else TRef k
    TProd l -> TProd $ subsTuple j t <$> l
    Tuple m -> Tuple $ subsSingle j t <$> m

  subsSingle j t sing = case sing of
    Wrap x -> Wrap $ subsSingle j t <$> x
    tup :. name -> subsTuple j t tup :. name


-- |Medium for SQL code
class (IsString t, Monoid t) => Medium t where
  inParens :: t -> t
  wSpace :: [t] -> t
  wComma :: [t] -> t
  wLines :: [t] -> t

-- Obvious instance for String
instance Medium String where
  inParens x = "(" <> x <> ")"
  wSpace = unwords
  wComma = intercalate ", "
  wLines = unlines

data SQLFormat f p t = SQLFormat {
  formVar :: Int -> t
  , formOver :: (Single f -> t) -> f (Single f) -> t
}

data SQLFormer f p t = SQLFormer {
  -- Single represented directly to SQL
  formSingle :: Single f -> t
  -- Tuple represented directly to SQL
  , formTuple :: Tuple f -> t
  -- Outmost join statement gets the inner formats with Just
  , formJoin :: SelJoin f p -> Maybe t -> t
  -- Select core with what to omit
  , formCore :: [String] -> SelCore f p -> t
  , formSel :: SelStmt f p -> t
}

formSQL :: (With1 Cond f, Functor f, Medium t) => SQLFormer f p t -> BuiltColumn f p -> t
formSQL former col = formSel former . runFresh $ col >>= procStmt

defFormer :: Medium t => SQLFormat f p t -> SQLFormer f p t -> SQLFormer f p t
defFormer form prev = SQLFormer {
  formSingle = \s -> let
    errAbsent n = error $ "Field " <> n <> " does not present in the tuple"
    errExtProd = error "Extracting from product tuple is not supported"
    in case s of
    Wrap f -> fOver (formSingle prev) f
    (TRef i) :. n -> fVar i <> fromString ("." <> n)
    Tuple l :. n -> maybe (errAbsent n) (formSingle prev) $ l M.!? n
    _ -> errExtProd

  , formTuple = \t -> let
    fSWith n s = wSpace [formSingle prev s, fromString "AS", fromString n]
    in case t of
    TRef i -> fVar i <> fromString ".*"
    TProd l -> wComma $ formTuple prev <$> l
    Tuple m -> wComma $ uncurry fSWith <$> M.toList m

  , formJoin = \SelJoin{ joinVar = var, joinSrc = src, joinOn = cond } inn -> let
    srcPart sig = stmt sig $ either fromString (inParens . formSel prev) src
    condPart sig = maybe [] (pure . stmt sig . formSingle prev) cond
    in case inn of
      Just inner -> wLines $ [wSpace [srcPart "FROM", fVar var], inner] <> condPart "WHERE"
      Nothing -> wLines $ [wSpace [srcPart "JOIN", fVar var]] <> condPart "ON"

  , formCore = \omit core -> case core of
    Values ts -> undefined -- TODO
  -- Absent from statements
    Selects t [] -> stmt "SELECT" $ formTuple prev (omit `omitFrom` t)
    Selects t (fs : js) -> wLines [stmt "SELECT" $ formTuple prev (omit `omitFrom` t)
      , froms] where
      -- From statement with succeeding joins
      froms = formJoin prev fs $ Just . wLines $ joinOn <$> js
      joinOn j = formJoin prev j Nothing
    -- TODO Combine as of now does not work for mixed combines - Join?
    Combines m s s' -> wLines [formCore prev omit s, comb m, formCore prev omit s'] where
      comb Union = fromString "UNION"
      comb Intersect = fromString "INTERSECT"
      comb Diff = fromString "EXCEPT"

  -- TODO Partition; Also order by should omit from partition if exists
  , formSel = \CProps{ colBody = b, colOrd = ords } -> let
    toName Asc = "ASC"; toName Dsc = "DSC"
    orders Omits{ ins = (dir, n) } = fromString $ unwords [n, toName dir]
    omitting = map ins . filter omits
    in wLines $ [formCore prev (snd <$> omitting ords) b]
    -- TODO Combine & Omit & Not represented using knowns -> Inside Join
    <> [stmt "ORDER BY" $ wComma $ fromString . orders <$> ords | not $ null ords]
} where
  SQLFormat{ formVar = fVar, formOver = fOver } = form
  stmt sig s = wSpace [fromString sig, s]
  omitFrom k t = case t of
    Tuple m -> Tuple $ foldr M.delete m k
    TProd l -> TProd $ omitFrom k <$> l
    TRef k -> TRef k

