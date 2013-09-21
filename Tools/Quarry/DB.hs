{-# LANGUAGE MultiParamTypeClasses #-}
module Tools.Quarry.DB
    ( run_
    -- * types
    , KeyData
    , KeyTag
    , KeyCategory
    , TagName
    , Tag(..)
    , DataCategory(..)
    -- * helper
    , withDB
    , dbFile
    -- * init
    , dbCreateTables
    -- * tag related query
    , dbGetCategories
    , dbCreateTag
    , dbFindTag
    , dbFindTagsMatching
    , dbResolveDigest
    , dbResolveKeyCategory
    , dbResolveKeyData
    , dbAddTag
    , dbRemoveTag
    , dbAddFile
    , dbFindWithTags
    , dbCommit
    ) where

import Tools.Quarry.Types
import Tools.Quarry.Monad
import Tools.Quarry.DBHelper
import Tools.Quarry.Config
import Tools.Quarry.DB.Types
import Control.Applicative
import Control.Monad (void)
import Control.Monad.Reader (ask)
import Control.Monad.Trans
import Data.Time.Clock
import Data.List (intercalate)
import Data.Word
import Data.Maybe
import Database.HDBC
import System.FilePath
import qualified Storage.HashFS as HFS
import qualified Data.ByteString.Char8 as BC

data DataCategory =
      CategoryPersonal
    | CategoryVideo
    | CategoryMusic
    | CategoryBook
    deriving (Show,Eq)

intDataCategory :: DataCategory -> Integer
intDataCategory CategoryPersonal = 1
intDataCategory CategoryVideo    = 2
intDataCategory CategoryMusic    = 3
intDataCategory CategoryBook     = 4

dbFile :: FilePath -> FilePath
dbFile root = root </> "quarry.db"

run_ :: (Functor m, MonadIO m) => QuarryDB -> String -> [SqlValue] -> m ()
run_ conn query args = void $ liftIO $ run conn query args

dbCreateTables :: QuarryDB -> IO ()
dbCreateTables conn = do
    mapM_ (flip (run conn) [])
        [ "CREATE TABLE version (ver INTEGER)"
        , "CREATE TABLE data (id INTEGER PRIMARY KEY, hash VARCHAR(80) NOT NULL, category INTEGER NOT NULL, description VARCHAR(1024), mtime INTEGER, dirname VARCHAR(4096), filename VARCHAR(1024), filetype INTEGER)"
        , "CREATE TABLE tag (id INTEGER PRIMARY KEY, name VARCHAR(128), category INTEGER NOT NULL)"
        , "CREATE TABLE category (id INTEGER PRIMARY KEY, name VARCHAR(256), abstract INTEGER NOT NULL)"
        , "CREATE TABLE tagmap (data_id INTEGER NOT NULL, tag_id INTEGER NOT NULL, UNIQUE(data_id, tag_id) ON CONFLICT REPLACE)"
        , "INSERT INTO version VALUES (1)"
        , "INSERT INTO category VALUES (1, 'personal', 0)"
        , "INSERT INTO category VALUES (2, 'video', 0)"
        , "INSERT INTO category VALUES (3, 'music', 0)"
        , "INSERT INTO category VALUES (4, 'book', 0)"
        ]
    commit conn

-- | Create a specific tag
--
-- will check if tag already exists
dbCreateTag :: Tag -> QuarryM KeyTag
dbCreateTag tag = do
    mt <- dbFindTag tag
    case mt of
        Nothing -> withDB $ \conn -> liftIO $ do
                        stmt <- prepare conn queryInsertTag
                        KeyTag <$> insertAndGetID conn stmt [toSql (tagName tag), toSql (tagCat tag)]
        Just t  -> return t
  where queryInsertTag = "INSERT INTO tag (name, category) VALUES (?, ?)"

dbGetCategories :: QuarryM [(KeyCategory, Category)]
dbGetCategories = withDB $ \conn -> liftIO $ getTableMap conn tableCategory KeyCategory toVal
  where toVal [SqlString name] = name
        toVal r                = error $ "unexpected value in category table: " ++ show r

-- | Try to find the key associated to a Tag
--
-- fix SQL escape
dbFindTag :: Tag -> QuarryM (Maybe KeyTag)
dbFindTag tag = withDB $ \conn -> do
    r <- liftIO $ quickQuery conn ("SELECT id FROM tag WHERE name='" ++ tagName tag ++ "' AND category='" ++ tagCat tag ++ "'") []
    case r of
        []      -> return Nothing
        [[uid]] -> return $ Just $ KeyTag $ fromSql uid
        _       -> error ("dbFindTag: " ++ show tag ++ " unexpected sql output format " ++ show r)

-- | Find all tags starting by a specific string
--
-- * fix SQL escape
--
-- * doesn't work if both Nothing
--
dbFindTagsMatching :: Maybe String -> Maybe Category -> QuarryM [(KeyCategory, TagName)]
dbFindTagsMatching nameConstraint categoryConstraint = withDB $ \conn -> do
    r <- liftIO $ quickQuery conn query []
    return $ map (\[rcat, rname] -> (KeyCategory $ fromSql rcat, fromSql rname)) r
  where query = "SELECT category, name FROM tag WHERE " ++
                maybe "" (\s -> "name LIKE '" ++ s ++ "%'") nameConstraint ++
                maybe "" (\s -> " AND category='" ++ s ++ "'") categoryConstraint

-- | add a Tag on some data
dbAddTag :: KeyData -> KeyTag -> QuarryM ()
dbAddTag key tag = withDB $ \conn -> liftIO $ prepare conn query >>= \stmt ->
                                              void $ execute stmt [toSql (getPrimaryKey key),toSql $ getPrimaryKey tag]
  where query = "INSERT INTO tagmap VALUES (?,?)"

-- | Remove a Tag from some data
dbRemoveTag :: KeyData -> KeyTag -> QuarryM ()
dbRemoveTag key tag = withDB $ \conn -> run_ conn query []
  where query = "DELETE FROM tagmap WHERE data_id=" ++ show key ++ " AND tag_id=" ++ show tag

-- | find digests that are part of every tags specified (intersection)
dbFindWithTags :: [Tag] -> QuarryM [QuarryDigest]
dbFindWithTags tags
    | null tags = error "cannot find with no tags"
    | otherwise = do
        tids <- catMaybes <$> mapM dbFindTag tags
        withDB $ \conn -> liftIO $ do
            stmt <- prepare conn (query tids)
            void $ execute stmt []
            fetchAllKeys charToDigest stmt
  where charToDigest (SqlString s)     = digestFromDb s
        charToDigest (SqlByteString s) = digestFromDb $ BC.unpack s
        charToDigest t                 = error $ "expecting string from data.hash got : " ++ show t
        query tids = intercalate " "
            [ "SELECT d.hash"
            , "FROM tagmap map, data d, tag t"
            , "WHERE map.tag_id = t.id"
            , "AND (t.id IN (" ++ (intercalate ", " $ map (show . getPrimaryKey) tids) ++ "))"
            , "AND d.id = map.data_id"
            , "GROUP BY d.id"
            -- without the HAVING COUNT it would be a union
            , "HAVING COUNT( d.id )=" ++ show (length tags)
            ]

dbResolveDigest :: QuarryDigest -> QuarryM (Maybe KeyData)
dbResolveDigest digest = do
    r <- withDB $ \conn -> liftIO $ quickQuery conn ("SELECT id FROM data WHERE hash='" ++ digestToDb digest ++ "'") []
    case r of
        [[uid]] -> return $ Just $ KeyData $ fromSql uid
        _       -> return Nothing

dbResolveKeyData :: KeyData -> QuarryM QuarryDigest
dbResolveKeyData fk = do
    r <- withDB $ \conn -> liftIO $ quickQuery conn ("SELECT hash FROM data WHERE id=" ++ show (getPrimaryKey fk)) []
    case r of
        [[uid]] -> return $ digestFromDb $ fromSql uid
        _       -> error ("data key " ++ show fk ++ " cannot be resolved")

dbResolveKeyCategory :: KeyCategory -> QuarryM Category
dbResolveKeyCategory fk = do
    r <- withDB $ \conn -> liftIO $ quickQuery conn ("SELECT name FROM category WHERE id=" ++ show (getPrimaryKey fk)) []
    case r of
        [[uid]] -> return $ fromSql uid
        _       -> error ("category key " ++ show fk ++ " cannot be resolved")

-- | add Digest to known content
dbAddFile :: QuarryDigest -> DataCategory -> FilePath -> (Word64, UTCTime) -> QuarryFileType -> QuarryM KeyData
dbAddFile digest dataCat path (_,_) ft
    | isRelative path = error "cannot accept relative path in dbAddFile"
    | otherwise = withDB $ \conn -> do
        let (dirName, fileName) = splitFileName path
        stmt <- liftIO $ prepare conn query
        liftIO $ do
            KeyData <$> insertAndGetID conn stmt
                [ toSql (digestToDb digest)
                , toSql (intDataCategory dataCat)
                , toSql (0::Int) -- fixme
                , toSql dirName
                , toSql fileName
                , toSql (fromEnum ft :: Int)]
  where query = "INSERT INTO data (hash, category, mtime, dirname, filename, filetype) VALUES (?,?,?,?,?,?)"

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
