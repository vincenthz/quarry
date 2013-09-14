module Tools.Quarry.DB
    ( run_
    , FileKey
    , withDB
    , dbFile
    , dbCreateTables
    -- * tag related query
    , dbCreateTag
    , dbFindTag
    , dbFindTagsStartingBy
    , dbResolveDigest
    , dbResolveKey
    , dbAddTag
    , dbRemoveTag
    , dbAddFile
    , dbFindWithTags
    , dbCommit
    ) where

import Tools.Quarry.Types
import Tools.Quarry.Monad
import Control.Monad (void, liftM)
import Control.Monad.Reader (ask)
import Control.Monad.Trans
import Data.Time.Clock
import Data.List (intercalate)
import Data.Word
import Database.HDBC
import System.FilePath
import qualified Storage.HashFS as HFS
import qualified Data.ByteString.Char8 as BC

type DataKey = Integer
type FileKey = DataKey -- deprecated alias
type TagKey = Integer

dbFile :: FilePath -> FilePath
dbFile root = root </> "quarry.db"

run_ :: (Functor m, MonadIO m) => QuarryDB -> String -> [SqlValue] -> m ()
run_ conn query args = void $ liftIO $ run conn query args

dbCreateTables :: QuarryDB -> IO ()
dbCreateTables conn = do
    mapM_ (flip (run conn) [])
        [ "CREATE TABLE version (ver INTEGER)"
        , "CREATE TABLE data (id INTEGER PRIMARY KEY, hash VARCHAR(80) NOT NULL, mtime INTEGER, dirname VARCHAR(4096), filename VARCHAR(1024), filetype INTEGER)"
        , "CREATE TABLE tag (id INTEGER PRIMARY KEY, name VARCHAR(128))"
        , "CREATE TABLE tagmap (data_id INTEGER NOT NULL, tag_id INTEGER NOT NULL, UNIQUE(data_id, tag_id) ON CONFLICT REPLACE)"
        , "INSERT INTO version VALUES (1)"
        ]
    commit conn

-- | Create a specific tag
--
-- will check if tag already exists
dbCreateTag :: Tag -> QuarryM TagKey
dbCreateTag name = do
    mt <- dbFindTag name
    case mt of
        Nothing -> withDB $ \conn -> liftIO $ do
                        stmt <- prepare conn queryInsertTag
                        insertAndGetID conn stmt [toSql name]
        Just t  -> return t
  where queryInsertTag = "INSERT INTO tag (name) VALUES (?)"

-- | Try to find the key associated to a Tag
--
-- fix SQL escape
dbFindTag :: Tag -> QuarryM (Maybe TagKey)
dbFindTag tag = withDB $ \conn -> do
    r <- liftIO $ quickQuery conn ("SELECT id FROM tag WHERE name='" ++ tag ++ "'") []
    case r of
        []      -> return Nothing
        [[uid]] -> return $ Just $ fromSql uid
        _       -> error ("dbFindTag: " ++ tag ++ " unexpected sql output format " ++ show r)

-- | Find all tags starting by a specific string
--
-- fix SQL escape
dbFindTagsStartingBy :: String -> QuarryM [Tag]
dbFindTagsStartingBy s = withDB $ \conn -> do
    r <- liftIO $ quickQuery conn ("SELECT name FROM tag WHERE name LIKE '" ++ s ++ "%'") []
    return $ map (\[x] -> fromSql x) r

-- | add a Tag on some data
dbAddTag :: DataKey -> TagKey -> QuarryM ()
dbAddTag key tag = withDB $ \conn -> liftIO $ prepare conn query >>= \stmt ->
                                              void $ execute stmt [toSql key,toSql tag]
  where query = "INSERT INTO tagmap VALUES (?,?)"

-- | Remove a Tag from some data
dbRemoveTag :: DataKey -> TagKey -> QuarryM ()
dbRemoveTag key tag = withDB $ \conn -> run_ conn query []
  where query = "DELETE FROM tagmap WHERE data_id=" ++ show key ++ " AND tag_id=" ++ show tag

-- | find digests that are part of every tags specified (intersection)
dbFindWithTags :: [Tag] -> QuarryM [QuarryDigest]
dbFindWithTags tags
    | null tags = error "cannot find with no tags"
    | otherwise = withDB $ \conn -> liftIO $ do
                    stmt <- prepare conn query
                    void $ execute stmt []
                    fetchAllKeys charToDigest stmt
  where charToDigest (SqlString s)     = digestFromDb s
        charToDigest (SqlByteString s) = digestFromDb $ BC.unpack s
        charToDigest t                 = error $ "expecting string from data.hash got : " ++ show t
        query = intercalate " "
            [ "SELECT d.hash"
            , "FROM tagmap map, data d, tag t"
            , "WHERE map.tag_id = t.id"
            , "AND (t.name IN (" ++ (intercalate ", " $ map quote tags) ++ "))"
            , "AND d.id = map.data_id"
            , "GROUP BY d.id"
            -- without the HAVING COUNT it would be a union
            , "HAVING COUNT( d.id )=" ++ show (length tags)
            ]
        -- FIXME can be escaped
        quote s = "'" ++ s ++ "'"

dbResolveDigest :: QuarryDigest -> QuarryM (Maybe FileKey)
dbResolveDigest digest = do
    r <- withDB $ \conn -> liftIO $ quickQuery conn ("SELECT id FROM data WHERE hash='" ++ digestToDb digest ++ "'") []
    case r of
        [[uid]] -> return $ Just $ fromSql uid
        _       -> return Nothing

dbResolveKey :: FileKey -> QuarryM QuarryDigest
dbResolveKey fk = do
    r <- withDB $ \conn -> liftIO $ quickQuery conn ("SELECT hash FROM data WHERE id=" ++ show fk) []
    case r of
        [[uid]] -> return $ digestFromDb $ fromSql uid
        _       -> error ("file key " ++ show fk ++ " cannot be resolved")

-- | add Digest to known content
dbAddFile :: QuarryDigest -> FilePath -> (Word64, UTCTime) -> QuarryFileType -> QuarryM DataKey
dbAddFile digest path (_,_) ft
    | isRelative path = error "cannot accept relative path in dbAddFile"
    | otherwise = withDB $ \conn -> do
        let (dirName, fileName) = splitFileName path
        stmt <- liftIO $ prepare conn query
        liftIO $ insertAndGetID conn stmt [toSql (digestToDb digest), toSql (0::Int), toSql dirName, toSql fileName, toSql (fromEnum ft :: Int)]
  where query = "INSERT INTO data (hash, mtime, dirname, filename, filetype) VALUES (?,?,?,?,?)"

dbCommit :: QuarryM ()
dbCommit = withDB $ \conn -> liftIO (commit conn)

-- FIXME probably no need to use the hexadecimal version
digestToDb :: QuarryDigest -> String
digestToDb = show

digestFromDb :: String -> QuarryDigest
digestFromDb = maybe (error "from db not a valid digest") id . HFS.inputDigest HFS.OutputHex

-- | execute something on a DB
withDB :: (QuarryDB -> QuarryM a) -> QuarryM a
withDB f = ask >>= \conf -> f (connection conf)

-- | execute a statement (that should be insert)
-- and return the last inserted rowid (primary key)
insertAndGetID :: QuarryDB -> Statement -> [SqlValue] -> IO Integer
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

