{-# LANGUAGE GADTSyntax #-}
module Data.Handle.SQLHandle (
  SelJoin(..), SelCore(..), SelStmt
  , Medium(..), SQLFormat(..), SQLFormer(..), formSQL, defFormer
) where

import Control.Applicative ( Alternative(..) )
import qualified Control.Monad.Trans as T
import Control.Monad.Trans.Cont ( evalContT )
import Control.Monad.Cont
import Control.Monad.State
import Data.List
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Maybe
import Data.Function ( on )
import Data.String ( IsString(..) )
import qualified Data.Map as M

import Data.Impl.Utils
import Data.Impl.RefTuple
import Data.Impl.Column
import Data.Column

-- |Aggregates in Singular statement
data SQLAg p a = Aggregate p [a]
-- |Refs in Singular statement
newtype SQLRef a = SQLRef String

type SQLReq f = (Field f
  , WithF Cond f
  , WithF SQLRef f
  , WithF (SQLAg (Aggregate f String)) f)

data SelJoin f t = SelJoin {
  joinVar :: Int
  , joinSrc :: Either String t
  , joinOn :: Maybe (Single f)
} deriving (Functor, Foldable, Traversable)

selJoin :: Int -> Either String t -> SelJoin f t
selJoin j s = SelJoin j s Nothing

data SelCore f p q where
  -- MAYBE just [Single f]?
  Values :: NonEmpty (M.Map String (Single f)) -> SelCore f p q
  Selects :: Tuple f -> [SelJoin f (SelWith f p q)] -> q -> SelCore f p q
  Combines :: CombineOp -> SelCore f p q -> SelCore f p q -> SelCore f p q
type SelWith f p q = WithProp p (SelCore f p q)

data SWindow f = SWindow{
  winVar :: Int
  , winOn :: Maybe (Single f)
  , winPart :: [Single f]
  , winOrd :: [(OrdDir, Single f)]
  , winSpec :: (RowPos, RowPos)
}

data SelAttrib f = SelAttrib{
  attrGroup :: Maybe ([Single f], Maybe (Single f)) -- TODO group by can't be with 0
  , attrWin :: Maybe (SWindow f)
}
instance Semigroup (SelAttrib f) where
  SelAttrib g w <> SelAttrib g' w' = SelAttrib (g <|> g') (w <|> w')
instance Monoid (SelAttrib f) where
  mempty = SelAttrib{ attrGroup = Nothing, attrWin = Nothing }

newtype SProps f = SProps{
  sOrder :: [(OrdDir, Single f)]
} deriving (Semigroup, Monoid) via [(OrdDir, Single f)]

type SelPre f = SelWith f (CProps f) ()
type SelStmt f = SelWith f (SProps f) (SelAttrib f)

---------------------------------------------------------------------------------

-- MAYBE with exception monads?
-- |Processes and simplifies statements, mainly Joins
procStmt :: (SQLReq f) => Column f -> Fresh (SelStmt f)
procStmt = evalContT . procBody >=> evalContT . procProps where
  procBody :: (SQLReq f) => Column f -> ContT (SelPre f) Fresh (SelPre f)
  procBody = traverse (fmap catStmts . simplify)

  simplify b = case b of
    Known n -> wrapJoin $ Left n
    Lift t -> pure $ Selects t [] ()
    Where _ -> error "Where statement cannot be the last"
    Combine m c c' -> Combines m <$> simplify c <*> simplify c'
    Join j c b -> callCC $ \exit -> do -- Uses continuation for early exit
      let pr = prop c where todo = "Window case"
      join <- case body c of
        -- Known to From, when no property applied
        Known n | isCPEmpty pr -> pure . selJoin j $ Left n
        -- Joining window-less lift leads to substitution
        --Lift t | isNothing (cWindow pr) -> simplify (subsBody j t b) >>= exit
        -- Reports Where
        Where s -> do
          -- MAYBE add join of Known "" and let other part do the job?
          postB <- simplify b
          res <- joining (selJoin whBind $ Left ""){ joinOn = Just s } postB
          exit res
        -- Use associativity to embed Join when it has no property
        Join k ci bi | isCPEmpty pr ->
          simplify (Join k ci $ Join j (fromBody bi) b) >>= exit
        -- Otherwise, simply joins the whole
        _ -> selJoin j . Right <$> procBody c
      (postB, wh) <- case b of
        -- Appends adjacent where to the join
        Join _ RunProp{ body = Where s } b' -> (, Just s) <$> simplify b'
        _ -> (, Nothing) <$> simplify b
      joining join{ joinOn = wh } postB

  whBind = 0
  -- Concatenations and Rewrites
  catStmts sel = case sel of
    -- When only values exist
    Selects (TMap m) [] _ -> Values $ m :| []
    -- Concatenates where statements into singular where
    Selects t js att -> Selects t after att where
      (wheres, others) = partition ((== whBind) . joinVar) js
      after = case (joinOn <$> wheres, others) of
        ([], _) -> others
        (_, []) -> error "Where statements can't be the only join"
        -- Appends wheres to the first join
        (whs, fs : os) -> fs{ joinOn = unionsWith condAnd (joinOn fs : whs) } : os
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
  
  -------------------------------------------------------------------------------

  -- |Processes properties - orders, partitions, windows
  procProps :: (SQLReq f) => SelPre f -> ContT (SelStmt f) Fresh (SelStmt f)
  procProps sel@(WithProp (cp, b)) = case b of
    -- Select statement, removes and changes accordingly
    Selects t js () -> callCC $ \exit -> do
      postJs <- traverse (traverse procProps) js
      case postJs of
        -- Simplifies single partitioned join, converting Where to Having
        [ join@SelJoin{ joinSrc = Right st@RunProp{ body = Selects ti jsi att } } ] ->
          when (isCPEmpty cp && isJust (attrGroup att)) (exit st')
          where
            st' = st{ body = Selects t' jsi att' }
            -- Substitutes internal tuple within outside factors, i.e. sel and cond
            t' = subsTuple (joinVar join) ti t
            co' = subsSingle (joinVar join) ti <$> joinOn join
            -- Lens could have made this more flexible
            SelAttrib{ attrGroup = Just (ind, ci) } = att
            att' = att{ attrGroup = Just (ind, unionWith condAnd ci co') }
        _ -> return ()
      (`evalStateT` t) $ do
        -- TODO Flip when windows present
        groups <- sequenceA $ uncurry procGrp <$> cPart cp
        ords <- traverse procOrd $ cOrd cp
        gets $ \t' -> RunProp {
          body = Selects t' postJs mempty{ attrGroup = (, Nothing) <$> groups }
          , prop = SProps{ sOrder = ords }
        }
      where
        -- Puts aggregates into tuple, and returns indices for groups
        procGrp grps aggs = do
          tup <- get
          let asAgg ag = Aggregate ag $ (tup :.) <$> elements ag
          put . TMap $ Wrap . wrap . asAgg <$> M.fromList aggs
          return $ (tup :.) <$> grps
        -- Omits from tuple while accumulating for orders
        --procOrd (Omits f r) = traverse procOmit $ Omits f <$> r
    -- Other statements with omits/partitions require joins
    -- MAYBE inspect if omitted is represented using known fields?
    _ | any omits (cOrd cp) || isJust (cPart cp) ->
      wrapJoin (Right sel) >>= procProps . fromBody
    -- Combine statement, simply apply recursively
    Combines m c c' -> do
      s <- procProps (fromBody c) >>= wrapJoin . Right
      s' <- procProps (fromBody c') >>= wrapJoin . Right
      return $ RunProp{ body = Combines m s s', prop = convProp cp }
    Values ts -> return $ RunProp{ body = Values ts, prop = convProp cp }

  -- Converts props, referring fields via their names
  convProp CProps{ cOrd = ord } =
    SProps{ sOrder = fmap (Wrap . wrap . SQLRef) . ins <$> ord }

  procOmit n = do
    st <- if omits n then state $ omitField (ins n) else pure Nothing
    maybe (gets (:. ins n)) return st
  omitField n tup = case tup of
    TMap m | M.member n m -> (Just (m M.! n), TMap $ M.delete n m)
    TProd l -> let (res, l') = unzip $ omitField n <$> l
      in (unionsWith const res, TProd l')
    _ -> (Nothing, tup)

  -------------------------------------------------------------------------------

  -- Joining join with existing core (join >>= core)
  joining join core = case core of
    Selects t js () -> pure $ Selects t (join : js) ()
    _ -> do k <- T.lift fresh
            pure $ Selects (TRef k) [join, selJoin k . Right $ fromBody core] ()

  -- Wraps target within a simple join, turning a select into a core
  wrapJoin sel = do
    k <- T.lift fresh
    return $ Selects (TRef k) [selJoin k sel] mempty

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
    TMap m -> TMap $ subsSingle j t <$> m

  subsSingle j t sing = case sing of
    Wrap x -> Wrap $ subsSingle j t <$> x
    tup :. name -> subsTuple j t tup :. name

  condAnd :: (WithCond f) => Single f -> Single f -> Single f
  condAnd = fmap wrapC . Cand `on` AsCond

---------------------------------------------------------------------------------

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
  , formJoin :: SelJoin f (SelStmt f) -> Maybe t -> Fresh t
  -- Select core
  , formCore :: SelCore f (SProps f) (SelAttrib f) -> Fresh t
  -- Select statement
  , formSel :: SelStmt f -> Fresh t
}

-- |Forms the SQL
formSQL :: (SQLReq f, Medium t) => SQLFormer f t -> BuiltColumn f -> t
formSQL former col = runFresh $ col >>= procStmt >>= formSel former

-- |Default former; fix it to get the former.
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
      TMap m -> Right <$> m M.!? n
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
    TMap m -> wComma $ uncurry fSWith <$> M.toAscList m

  , formJoin = \SelJoin{ joinVar = var, joinSrc = src, joinOn = cond } inn -> do
    srcP <- either (pure . fromString) (fmap inParens . formSel prev) src
    let condPart sig = maybe [] (pure . stmt sig . formSingle prev) cond
    let srcPart sig = stmt sig srcP
    pure $ case inn of
      Just inner -> wLines $ [wSpace [srcPart "FROM", fVar var], inner] <> condPart "WHERE"
      Nothing -> wLines $ [wSpace [srcPart "JOIN", fVar var]] <> condPart "ON"

  , formCore = \core -> case core of
    -- MAYBE Perhaps handle values upfront?
    -- Value statement with only one element is represented as single select
    Values (tup :| []) -> pure $ singleSel (TMap tup)
    -- Value statements; Special treatment to first value as some sql lacks a way to name
    Values (tup :| ts) -> pure $ wSpace [singleSel (TMap tup)
      , fromString "UNION", stmt "VALUES" . wComma $ value <$> ts] where
      value m = inParens . wComma $ formSingle prev <$> M.elems m
    -- Absent from statements
    Selects t [] att -> pure . wLines $ [singleSel t] <> attList att
    -- With from/join statements
    Selects t (fs : js) att -> do
      joins <- traverse (\j -> formJoin prev j Nothing) js
      froms <- formJoin prev fs $ Just (wLines joins)
      pure . wLines $ [singleSel t, froms] <> attList att
    -- TODO Combine as of now does not work for mixed combines - Join?
    -- Adds to the rightmost
    Combines m s s' -> do
      sP <- formCore prev s
      s'P <- (>>= formCore prev) $ case s' of
        -- Due to standard SQL's limitation, rightmost combine is joined first
        Combines{} -> do
          k <- fresh
          pure $ Selects (TRef k) [selJoin k $ Right . fromBody $ s'] mempty
        _ -> pure s'
      pure $ wLines [sP, comb m, s'P]
      where
        comb Union = fromString "UNION"
        comb Intersect = fromString "INTERSECT"
        comb Diff = fromString "EXCEPT"

  , formSel = \RunProp{ body = b, prop = SProps{ sOrder = ords } } -> do
    let toName Asc = fromString "ASC"; toName Dsc = fromString "DSC"
    let orders (dir, s) = wSpace [formSingle prev s, toName dir]
    coreP <- formCore prev b
    pure . wLines $ [coreP]
      <> [stmt "ORDER BY" $ wComma $ orders <$> ords | not $ null ords]
} where
  SQLFormat{ formVar = fVar, formOver = fOver } = form
  stmt sig s = wSpace [fromString sig, s]
  singleSel t = stmt "SELECT" $ formTuple prev t
  attList att = maybe [] pure (grpBy <$> attrGroup att)
  grpBy (inds, cond) = wSpace $ [stmt "GROUP BY" . wComma $ formSingle prev <$> inds]
    <> maybe [] (pure . stmt "HAVING" . formSingle prev) cond
