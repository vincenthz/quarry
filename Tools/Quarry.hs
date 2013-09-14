module Tools.Quarry
    ( initialize
    , QuarryConfig
    , runQuarry
    , importFile
    , updateDigest
    , resolveDigest
    , findDigestWithTags
    , readDigest
    , findTags
    ) where

import qualified Storage.HashFS as HFS
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Crypto.Hash (SHA512)

import System.FilePath
import System.Directory

import Database.HDBC.Sqlite3 (connectSqlite3)

import Tools.Quarry.Types
import Tools.Quarry.Monad
import Tools.Quarry.DB
import Data.FileFormat

readDigest :: String -> Maybe QuarryDigest
readDigest s = HFS.inputDigest HFS.OutputHex s

runHFS :: HFS.HashFS SHA512 a -> QuarryM a
runHFS f = ask >>= \conf -> liftIO $ HFS.run f (hashfsConf conf)

--getRootPath = runHFS (HFS.hashfsRoot <$> ask)

initialize :: Bool -> FilePath -> IO QuarryConfig
initialize wantNew root
    | wantNew = do
        hasDb <- doesFileExist (dbFile root)
        when hasDb $ error "look like it's already initialized"
        HFS.run HFS.initialize quarryHashFSConf
        conn <- connectSqlite3 (dbFile root)
        dbCreateTables conn
        return $ QuarryConfig { connection = conn, hashfsConf = quarryHashFSConf }
    | otherwise = do
        hasDb <- doesFileExist (dbFile root)
        when (not hasDb) $ error "look like no DB"
        conn <- connectSqlite3 (dbFile root)
        return $ QuarryConfig { connection = conn, hashfsConf = quarryHashFSConf }
  where quarryHashFSConf = HFS.makeConfSHA512 [2] HFS.OutputHex root

importFile :: [Tag] -> FilePath -> QuarryM QuarryDigest
importFile tags rfile = do
    current <- liftIO getCurrentDirectory
    let file = if isRelative rfile then current </> rfile else rfile
    (digest,info) <- runHFS $ do
                    digest <- HFS.importFile file
                    info   <- HFS.readInfo digest
                    case info of
                        Nothing -> error ("import of file " ++ file ++ " failed")
                        Just z  -> return (digest, z)
    ty <- liftIO $ autoFileType file
    k  <- dbAddFile digest file info ty
    when (not $ null tags) $ do
        mapM_ (dbCreateTag >=> dbAddTag k) tags
    dbCommit
    return digest
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

resolveDigest :: QuarryDigest -> QuarryM (Maybe FileKey)
resolveDigest digest = dbResolveDigest digest

findDigestWithTags :: [Tag] -> QuarryM [QuarryDigest]
findDigestWithTags tags = dbFindWithTags tags

-- | find tags with specific queries
--
-- at the moment only 'starting by' query supported
findTags :: String -> QuarryM [Tag]
findTags s =
    -- need ending by, contains, etc..
    dbFindTagsStartingBy s

--digestOfKeys :: [DataKey] -> QuarryM [QuarryDigest]
--digestOfKeys fks = mapM dbResolveKey fks
