{-# LANGUAGE GADTSyntax #-}
module Data.Handle.SQLHandle (
  SelJoin(..), SelCore(..), SelStmt
  , Medium(..), SQLFormat(..), SQLFormer(..), formSQL, defFormer
) where

import Control.Applicative
import Control.Monad.State
import Data.List
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Maybe
import Data.Function ( on )
import Data.String ( IsString(..) )
import qualified Data.Map as M

import Data.Impl.Utils
import Data.Impl.Column
import Data.Column

-- |Refs and Aggregates in Singular statement. Impl-only.
data SQLAg p a = Aggregate p [a]

type SQLReq f = (Field f, WithF Cond f, WithF (SQLAg (Aggregate f)) f)

data SelJoin f t = SelJoin {
  joinVar :: Int
  , joinSrc :: Either String t
  , joinOn :: Maybe (Single f)
} deriving Functor

selJoin :: Int -> Either String t -> SelJoin f t
selJoin j s = SelJoin j s Nothing

data SelCore f p q where
  Values :: NonEmpty (M.Map String (Single f)) -> SelCore f p q
  Selects :: Tuple f -> [SelJoin f (SelWith f p q)] -> q -> SelCore f p q
  Combines :: CombineOp -> SelCore f p q -> SelCore f p q -> SelCore f p q
type SelWith f p q = WithProp p (SelCore f p q)

data SelAttrib f = SelAttrib{
  attrGroup :: Maybe ([Single f], Maybe (Single f))
  , foo :: Maybe ()
}
defAtt :: SelAttrib f
defAtt = SelAttrib{ attrGroup = Nothing, foo = Nothing }

data SProps f = SProps{
  sOrder :: [(OrdDir, Single f)]
  , sGroups :: Maybe ([Single f], Maybe (Single f)) -- TODO GROUP BY should be in Selects
  , sWindow :: Maybe (RowPos, RowPos) -- TODO At least better than this
}

type SelPre f = SelWith f (CProps f) ()
type SelStmt f = SelWith f (SProps f) (SelAttrib f)

-- |Processes and simplifies statements, mainly Joins
procStmt :: (SQLReq f) => Column f -> Fresh (SelStmt f)
procStmt = fmap procProps . procBody where
  procBody :: (SQLReq f) => Column f -> Fresh (SelPre f)
  procBody = traverse (fmap catStmts . simplify)

  simplify body = case body of
    -- Non-join statements
    Known n -> do k <- fresh; pure $ Selects (TRef k) [selJoin k $ Left n] ()
    Lift t -> pure $ Selects t [] ()
    Where _ -> error "Where statement cannot be the last"
    Combine m c c' -> Combines m <$> simplify c <*> simplify c'

    -- Partitioned column cannot be simplified
    -- TODO In fact, window require simplifications; todo for later
    Join j c@RunProp{ prop = CProps{ cPart = Just _ } } b -> do
      stmt <- Right <$> procBody c
      simplify b >>= joining (selJoin j stmt)
    -- Order without partition is discarded with single statement joins

    -- Known-Where: groups the two into join
    Join j RunProp{ body = Known n } (Join _ RunProp{ body = Where s } b) ->
      simplify b >>= joining (selJoin j $ Left n){ joinOn = Just s }
    -- Known to From
    Join j RunProp{ body = Known n } b ->
      simplify b >>= joining (selJoin j $ Left n)
    -- Joining Lift simply leads to substitutions of latter statements
    Join j RunProp{ body = Lift t } b ->
      simplify (subsBody j t b)
    -- Gathers where statement for use
    Join _ RunProp{ body = Where s } b ->
      simplify b >>= joining (selJoin whBind $ Left ""){ joinOn = Just s }
    -- Using associativity to embed Join when it has no property
    Join j RunProp{ body = Join k ci bi, prop = p } b | isCPEmpty p ->
      simplify $ Join k ci $ Join j (fromBody bi) b
    -- Join-Where: groups the two into join (TODO)
    
    -- Otherwise, do not simplify
    Join j c b -> do
      stmt <- Right <$> procBody c
      simplify b >>= joining (selJoin j stmt)

  whBind = 0
  -- Concatenates and Rewrites
  catStmts sel = case sel of
    -- When only values exist
    Selects (Tuple m) [] _ -> Values $ m :| []
    -- Concatenates where statements into singular where
    Selects t js att -> Selects t after att where
      (wheres, others) = partition ((== whBind) . joinVar) js
      after = case (joinOn <$> wheres, others) of
        ([], _) -> others
        (_, []) -> error "Where statements can't be the only ones"
        (whs, fs : os) -> fs{ joinOn = catWheres (joinOn fs : whs) } : os
    -- Right first to Left first
    Combines m c (Combines m' c' c'') | m == m' && m /= Diff ->
      catStmts $ Combines m (Combines m c c') c''
    -- Recursive application for Combine, as simplify itself does not apply to all
    Combines m c c' -> let cs = catStmts c; cs' = catStmts c'
      in case (m, cs, cs') of
        -- Concatenates singular tuples into values
        (Union, Values l, Values l') -> Values $ l <> l'
        _ -> Combines m cs cs'
    Values{} -> sel -- Nothing to concat

  -- Joining join with existing core
  joining join core = case core of
    Selects t js () -> pure $ Selects t (join : js) ()
    _ -> do
      k <- fresh; pure $ Selects (TRef k) [join, selJoin k . Right $ pure core] ()

  -- |Processes properties - orders, partitions, windows
  procProps :: (SQLReq f) => SelPre f -> SelStmt f
  procProps = go where
    go (WithProp (CProps{ cOrd = ord, cPart = part, cWindow  = win }
      , body)) = case body of
      -- Simplifies Single Partitioned Join, converting Where to Having
      Selects t [ SelJoin{ joinVar = j
        , joinSrc = Right st@RunProp{ prop = CProps{ cPart = Just _, cWindow = Nothing }}
        , joinOn = cond } ] _
        | null ord && isNothing part && isNothing win ->
          RunProp{ body = Selects t'' js att, prop = pr{ sGroups = Just (l, catWheres [c, cond]) } }
        where
          RunProp{ body = Selects t' js att
          , prop = pr@SProps{ sGroups = Just (l, c) } } = procProps st
          t'' = subsTuple j t' t -- TODO Requires deeper substitution
      -- Select statement; removes and changes accordingly
      Selects t js () -> (`evalState` t) $ do
        -- TODO Flip when windows present
        groups <- sequenceA $ uncurry procGrp <$> part
        ords <- traverse procOrd ord
        gets $ \t' -> RunProp {
          body = Selects t' (fmap procProps <$> js) defAtt
          , prop = SProps{
            sOrder = ords, sGroups = (, Nothing) <$> groups, sWindow = Nothing }
        } where
          procGrp :: (SQLReq f) => [String] -> [(String, Aggregate f)] -> State (Tuple f) [Single f]
          procGrp grps aggs = do
            tup <- get
            undefined -- TODO Handle aggregates into tuple here
            return $ (tup :.) <$> grps

          procOrd (Omits f r) = traverse procOmit $ Omits f <$> r          
      -- Combine/Value statement; If omitted one exists, join a table. TODO
      _ -> undefined where todo = 0
    procOmit n = do
      st <- if omits n then state $ omitField (ins n) else pure Nothing
      maybe (gets (:. ins n)) return st
    omitField n tup = case tup of
      Tuple m | M.member n m -> (Just (m M.! n), Tuple $ M.delete n m)
      TProd l -> let (res, l') = unzip $ omitField n <$> l
        in (unionsWith const res, TProd l')
      _ -> (Nothing, tup)

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

  catWheres :: (WithCond f) => [Maybe (Single f)] -> Maybe (Single f)
  catWheres = unionsWith (fmap wrapC . Cand `on` AsCond)


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

data SQLFormat f t = SQLFormat {
  formVar :: Int -> t
  , formOver :: (Single f -> t) -> f (Single f) -> t
}

data SQLFormer f t = SQLFormer {
  -- Single represented directly to SQL
  formSingle :: Single f -> t
  -- Tuple represented directly to SQL
  , formTuple :: Tuple f -> t
  -- Outmost join statement gets the inner formats with Just
  , formJoin :: SelJoin f (SelStmt f) -> Maybe t -> t
  -- Select core
  , formCore :: SelCore f (SProps f) (SelAttrib f) -> t
  -- Select statement
  , formSel :: SelStmt f -> t
}

formSQL :: (SQLReq f, Medium t) => SQLFormer f t -> BuiltColumn f -> t
formSQL former col = formSel former . runFresh $ col >>= procStmt

defFormer :: Medium t => SQLFormat f t -> SQLFormer f t -> SQLFormer f t
defFormer form prev = SQLFormer {
  formSingle = \s -> let
    errAbsent n = error $ "Field " <> n <> " does not present in the tuple"
    prior (Right x) _ = Right x
    prior _ (Right x) = Right x
    prior (Left y) _ = Left y
    findField n tup = case tup of
      TRef k -> Just $ Left k
      TProd l -> unionsWith prior $ findField n <$> l
      Tuple m -> Right <$> m M.!? n
    in case s of
    Wrap f -> fOver (formSingle prev) f
    tup :. n -> case findField n tup of
      Nothing -> errAbsent n
      Just (Left i) -> fVar i <> fromString ("." <> n)
      Just (Right s') -> formSingle prev s'

  , formTuple = \t -> let
    fSWith n s = wSpace [formSingle prev s, fromString "AS", fromString n]
    in case t of
    TRef i -> fVar i <> fromString ".*"
    TProd l -> wComma $ formTuple prev <$> l
    Tuple m -> wComma $ uncurry fSWith <$> M.toAscList m

  , formJoin = \SelJoin{ joinVar = var, joinSrc = src, joinOn = cond } inn -> let
    srcPart sig = stmt sig $ either fromString (inParens . formSel prev) src
    condPart sig = maybe [] (pure . stmt sig . formSingle prev) cond
    in case inn of
      Just inner -> wLines $ [wSpace [srcPart "FROM", fVar var], inner] <> condPart "WHERE"
      Nothing -> wLines $ [wSpace [srcPart "JOIN", fVar var]] <> condPart "ON"

  , formCore = \core -> case core of
    -- Value statement with only one element is represented as single select
    Values (tup :| []) -> formCore prev $ Selects (Tuple tup) [] defAtt
    -- Value statements; Special treatment to first value as some sql lacks a way to name
    Values (tup :| ts) -> wSpace [formCore prev $ Selects (Tuple tup) [] defAtt
      , fromString "UNION", stmt "VALUES" . wComma $ value <$> ts] where
      value m = inParens . wComma $ formSingle prev <$> M.elems m
    -- Absent from statements
    Selects t [] att -> stmt "SELECT" $ formTuple prev t
    Selects t (fs : js) att -> wLines [stmt "SELECT" $ formTuple prev t
      , froms] where
      -- From statement with succeeding joins
      froms = formJoin prev fs $ Just . wLines $ joinOn <$> js
      joinOn j = formJoin prev j Nothing
    -- TODO Combine as of now does not work for mixed combines - Join?
    Combines m s s' -> wLines [formCore prev s, comb m, formCore prev s'] where
      comb Union = fromString "UNION"
      comb Intersect = fromString "INTERSECT"
      comb Diff = fromString "EXCEPT"

  , formSel = \RunProp{ body = b, prop = SProps{ sOrder = ords } } -> let
    toName Asc = fromString "ASC"; toName Dsc = fromString "DSC"
    orders (dir, s) = wSpace [formSingle prev s, toName dir]
    todo = error "Group By" -- TODO
    in wLines $ [formCore prev b]
    -- TODO Combine & Omit & Not represented using knowns -> Inside Join
    <> [stmt "ORDER BY" $ wComma $ orders <$> ords | not $ null ords]
} where
  SQLFormat{ formVar = fVar, formOver = fOver } = form
  stmt sig s = wSpace [fromString sig, s]
