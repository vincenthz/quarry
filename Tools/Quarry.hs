module Tools.Quarry
    ( initialize
    , KeyCategory
    , KeyTag
    , TagName
    , Tag(..)
    , ImportType(..)
    , DataCategory(..)
    , QuarryConfig
    , runQuarry
    , importFile
    , updateDigest
    , resolveDigest
    , resolveTag
    , getCategoryTable
    , findDigestWithTags
    , readDigest
    , findTags
    , exist
    , computeDigest
    , addCategory
    , QuarryInfo(..)
    , getInfo
    ) where

import Storage.HashFS (ImportType(..))
import qualified Storage.HashFS as HFS
import System.Posix.Files
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Crypto.Hash (SHA512)

import System.FilePath
import System.Directory
import Data.Word

import Data.Time.Clock.POSIX

import Database.HDBC.Sqlite3 (connectSqlite3)

import Tools.Quarry.Types
import Tools.Quarry.Cache
import Tools.Quarry.Config
import Tools.Quarry.Monad
import Tools.Quarry.DB
import Tools.Quarry.DBHelper
import Data.FileFormat

readDigest :: String -> Maybe QuarryDigest
readDigest s = HFS.inputDigest HFS.OutputHex s

runHFS :: HFS.HashFS SHA512 a -> QuarryM a
runHFS f = ask >>= \conf -> liftIO $ HFS.run f (hashfsConf conf)

--getRootPath = runHFS (HFS.hashfsRoot <$> ask)
exist :: QuarryDigest -> QuarryM Bool
exist digest = runHFS (maybe False (const True) <$> HFS.readInfo digest)

computeDigest :: FilePath -> QuarryM QuarryDigest
computeDigest file = runHFS (HFS.computeHash file)

initialize :: Bool -> FilePath -> IO QuarryConfig
initialize wantNew root = do
    hasDb <- doesFileExist (dbFile root)
    if wantNew
        then do when hasDb $ error "look like it's already initialized"
                HFS.run HFS.initialize quarryHashFSConf
        else when (not hasDb) $ error "look like no DB"
    conn <- connectSqlite3 (dbFile root)
    when wantNew $ dbCreateTables conn
    cache <- emptyCache
    return $ QuarryConfig { connection = conn, hashfsConf = quarryHashFSConf, cacheTags = cache }
  where quarryHashFSConf = HFS.makeConfSHA512 [2] HFS.OutputHex root

importFile :: ImportType -> DataCategory -> Maybe POSIXTime -> Maybe FilePath -> [Tag] -> FilePath -> QuarryM (QuarryDigest,Bool)
importFile itype dataCat mDate mFilename tags rfile = do
    current <- liftIO getCurrentDirectory
    let file = if isRelative rfile then current </> rfile else rfile
    fstat <- liftIO $ getFileStatus file
    digest <- runHFS $ do
                    digest <- HFS.importFile itype file
                    info   <- HFS.readInfo digest
                    case info of
                        Nothing -> error ("import of file " ++ file ++ " failed")
                        Just _  -> return digest
    key <- dbResolveDigest digest
    case key of
        Nothing -> do
            ty <- liftIO $ autoFileType file
            let info = (fromIntegral $ fileSize fstat, realToFrac $ modificationTime fstat)
            k  <- dbAddFile digest dataCat (maybe file id mFilename) mDate info ty
            when (not $ null tags) $ do
                mapM_ (dbCreateTag >=> dbAddTag k) tags
            dbCommit
            return (digest, True)
        Just _ -> return (digest, False)
  where autoFileType path = toQuarryFileType <$> getFileformat path
        toQuarryFileType ft = case ft of
                FT_JPEG   -> QuarryTypeImage
                FT_PNG    -> QuarryTypeImage
                FT_PDF _  -> QuarryTypeDocument
                FT_MP3    -> QuarryTypeSound
                FT_OGG    -> QuarryTypeSound
                FT_RIFF   -> QuarryTypeImage -- webp .. could be avi !
                FT_Text   -> QuarryTypeDocument
                _         -> QuarryTypeUnknown

updateDigest :: QuarryDigest -> [Tag] -> [Tag] -> QuarryM  ()
updateDigest digest addTags delTags = do
    mfk <- dbResolveDigest digest
    case mfk of
        Nothing -> error "digest not found"
        Just fk -> do
            mapM_ (dbCreateTag >=> dbAddTag fk) addTags
            mapM_ (dbFindTag >=> maybe (return ()) (dbRemoveTag fk)) delTags
            dbCommit

resolveDigest :: QuarryDigest -> QuarryM (Maybe KeyData)
resolveDigest digest = dbResolveDigest digest

resolveTag :: Either TagName Tag -> QuarryM (Maybe Tag)
resolveTag (Left tname) = do
    r <- dbFindTagsMatching (Just tname) Nothing
    case r of
        [(cat,tname2)] -> dbResolveKeyCategory cat >>= \c -> return $ Just $ Tag { tagCat = c, tagName = tname2 }
        _              -> return Nothing
resolveTag (Right tag) = return $ Just tag

findDigestWithTags :: [Tag] -> QuarryM [QuarryDigest]
findDigestWithTags tags = dbFindWithTags tags

-- | find tags with specific queries
--
-- at the moment only 'starting by' query supported
findTags :: Maybe String -> Maybe Category -> QuarryM [(KeyCategory, TagName)]
findTags s mcat =
    -- need ending by, contains, etc..
    dbFindTagsMatching s mcat

getCategoryTable :: QuarryM [(KeyCategory,(Category,Bool))] 
getCategoryTable = dbGetCategories

addCategory :: Category -> QuarryM ()
addCategory cat = do
    _ <- dbCreateCategory cat
    dbCommit
    return ()

data QuarryInfo = QuarryInfo
    { infoNFile     :: Word64
    , infoNTag      :: Word64
    , infoNCategory :: Word64
    } deriving (Show,Eq)

getInfo :: QuarryM QuarryInfo
getInfo = withDB $ \conn -> liftIO $
    QuarryInfo <$> getCount conn tableData Nothing
               <*> getCount conn tableTag Nothing
               <*> getCount conn tableCategory Nothing

--digestOfKeys :: [DataKey] -> QuarryM [QuarryDigest]
--digestOfKeys fks = mapM dbResolveKey fks
