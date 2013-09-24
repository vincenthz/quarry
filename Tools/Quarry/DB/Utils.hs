module Tools.Quarry.DB.Utils
    where

import Tools.Quarry.Types
import Tools.Quarry.Monad
import Tools.Quarry.Config
import Control.Monad (void)
import Control.Monad.Reader (ask)
import Control.Monad.Trans
import Database.HDBC
import Data.List (intercalate)
import qualified Storage.HashFS as HFS

-- | execute something on a DB
withDB :: (QuarryDB -> QuarryM a) -> QuarryM a
withDB f = ask >>= \conf -> f (connection conf)

-- | run a query
run_ :: (Functor m, MonadIO m) => QuarryDB -> String -> [SqlValue] -> m ()
run_ conn query args = void $ liftIO $ run conn query args

-- | commit database queries
dbCommit :: QuarryM ()
dbCommit = withDB $ \conn -> liftIO (commit conn)

-- FIXME probably no need to use the hexadecimal version
digestToDb :: QuarryDigest -> String
digestToDb = show

digestFromDb :: String -> QuarryDigest
digestFromDb = maybe (error "from db not a valid digest") id . HFS.inputDigest HFS.OutputHex


sqlSelect :: String -> String -> Maybe String -> String
sqlSelect sel tableS constraint =
    "SELECT " ++ sel ++ " FROM " ++ tableS ++ maybe "" (\s -> " WHERE " ++ s) constraint

sqlInsert :: String -> [String] -> [String] -> String
sqlInsert tableS fields values
    | not (null fields) && length fields /= length values = error ("sql insert: " ++ tableS ++ " mismatch length of fields and values : " ++ show fields ++ " " ++ show values)
    | otherwise = "INSERT INTO " ++ tableS ++ fieldS ++ " VALUES (" ++ valuesS ++ ")"
  where fieldS | null fields = ""
               | otherwise   = "(" ++ intercalate ", " fields ++ ")"
        valuesS = intercalate ", " values
