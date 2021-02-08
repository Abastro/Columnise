module Data.Handle.SQLiteHandle where

{-
import Data.List ( intercalate )
import Data.String ( IsString(..) )
import Data.Text ( Text )
import Data.Text.Lazy.Builder ( Builder, singleton )
import Data.Text.Lazy.Builder.Int ( decimal )

import Database.SQLite3
import Data.Impl.Column ( CombineOp(..), Order(..), ColProps(..) )
import Data.Handle.SQLHandle


genStmt :: Database -> SelStmt SingleStmt p -> IO ()
genStmt db sel = undefined
  where
    selStmt :: SelStmt f p -> Builder
    selStmt Props{ body = b, colOrd = r } =
      coreStmt b <> case r of
        [] -> mempty
        ords -> lining . fromString $ "ORDER BY " <> intercalate ", " (orders <$> ords)
      where
        orders (Asc x) = unwords [x, "ASC"]
        orders (Dsc x) = unwords [x, "DSC"]
        orders (Omit r) = orders r

    coreStmt body = case body of
      Selects t [] -> stmt "SELECT" (undefined t)
      Selects t (fs : js) -> stmt "SELECT" (undefined t) <> froms where
        froms = joinStmt "FROM" "WHERE" joinOns fs
        joinOns = mconcat $ joinStmt "JOIN" "ON" mempty <$> js
      Combines Union s s' -> selStmt s <> lining (fromString "UNION") <> lining (selStmt s')
      Combines Intersect s s' -> selStmt s <> lining (fromString "INTERSECTION") <> lining (selStmt s')
      Combines Diff s s' -> selStmt s <> lining (fromString "DIFFERENCE") <> lining (selStmt s')

    joinStmt join on btwn SelJoin{
      joinVar = var, joinSrc = src, joinOn = cond
      } = srcPart <> namePart <> btwn <> condPart where
        srcPart = lining . stmt join $ either fromString (parens . selStmt) src
        namePart = spacing . stmt "AS" $ varName var
        condPart = maybe mempty (lining . stmt on . undefined) cond

    stmt key v = fromString key <> singleton ' ' <> v
    varName n = singleton 'a' <> decimal n

    parens x = singleton '(' <> x <> singleton ')'
    spacing x = singleton ' ' <> x
    lining x = singleton '\n' <> x
-}


{-
sqliteHandle :: Database -> SQLHandle
sqliteHandle db = SQLHandle {
  viewTable = \table -> do
    stmt <- prepare db $ fromString "select y from T where x == ?0;"
    bindInt stmt 0 0
    _ <- step stmt
    meh <- columnInt64 stmt 0
    finalize stmt
    pure undefined
  , insertOn = undefined
  , updateOn = undefined
}
-}
