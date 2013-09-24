{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Tools.Quarry.DBHelper
    where

import Control.Monad (liftM)
import Database.HDBC
import Tools.Quarry.DB.Types
import Data.Word

data StringQuery = StartWith String | EndWith String | Contains String | Is String
    deriving (Eq)

-- FIXME escapable s
instance Show StringQuery where
    show (StartWith s) = "'" ++ s ++ "%'"
    show (EndWith s)   = "'%" ++ s ++ "'"
    show (Contains s)  = "'%" ++ s ++ "%'"
    show (Is s)        = "'" ++ s ++ "'"

{-
data Selector =
      SqlEq String String
    | SqlNe
    | SqlGt
    | SqlLt
    | SqlGe
    | SqlGt String String
    | SqlBetween String String
    | SqlLike String String
    | SqlIn String [String]

sqlStar = ["*"]

sqlSelect :: [Field] -> Table ->
sqlSelect fields table =

sqlWhere :: String -> String -> String
sqlWhere query wh = query ++ " WHERE " ++ wh

sqlSelect ["id"] "tag" `sqlWhere` "name = "
-}

-- | execute a statement (that should be insert)
-- and return the last inserted rowid (primary key)
insertAndGetID :: IConnection con => con -> Statement -> [SqlValue] -> IO Integer
insertAndGetID conn stmt values = do
    _ <- execute stmt values
    [[uid]] <- quickQuery conn "SELECT last_insert_rowid()" []
    return $ fromSql uid

-- fetch all rows of 1 column and apply a mapping function f on each element
fetchAllKeys :: (SqlValue -> a) -> Statement -> IO [a]
fetchAllKeys f stmt = loop
  where loop = do mr <- fetchRow stmt
                  case mr of
                      Nothing  -> return []
                      Just [r] -> liftM (f r:) loop
                      Just _   -> error "internal error: fetchAllKeys expect just 1 row"

-- | fetch all rows and apply a mapping on each element
fetchAll :: ([SqlValue] -> a) -> Statement -> IO [a]
fetchAll f stmt = loop
  where loop = do mr <- fetchRow stmt
                  case mr of
                      Nothing   -> return []
                      Just vals -> liftM (f vals:) loop

-- | return an association list of all the rows 
getTableMap :: IConnection con => con -> Table -> (Integer -> a) -> ([SqlValue] -> b) -> IO [(a,b)]
getTableMap conn table keyConstr dataConstr = do
    stmt <- prepare conn query
    _ <- execute stmt []
    fetchAll toTable stmt
  where
    query = "SELECT * FROM " ++ tableName table
    toTable []    = error "empty row"
    toTable (k:v) = (keyConstr $ fromSql k, dataConstr v)

getCount :: IConnection con => con -> Table -> Maybe String -> IO Word64
getCount conn table constraint = do
    [[count]] <- quickQuery conn query []
    return $ fromSql count
  where query = "SELECT COUNT(*) FROM " ++ tableName table ++ maybe "" (\s -> " WHERE " ++ s) constraint
