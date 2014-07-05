module Storage.HashFS
    ( run
    , initialize
    , HashFS
    , HashFSConf(..)
    , makeConf
    , makeConfSHA512
    , OutputDesc(..)
    , outputDigest
    , inputDigest
    , computeHash
    , onDigestFile
    , ImportType(..)
    , importFile
    , importFileAt
    , deleteFile
    , readFile
    , readInfo
    , verify
    , exists
    , iterateFiles
    , getPath
    ) where

import Data.Word (Word64)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Control.Applicative
import Control.Monad (when, unless, foldM, filterM, forM_)
import Control.Monad.Trans
import Control.Monad.Reader (ask)
import Storage.HashFS.Types
import Storage.HashFS.Monad
import Storage.HashFS.Path
import Storage.HashFS.Utils
import Crypto.Hash
import System.IO hiding (readFile)
import Prelude hiding (readFile)
import Data.Time.Clock.POSIX (POSIXTime)

import System.Directory (createDirectoryIfMissing, removeFile, doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>), dropTrailingPathSeparator, dropFileName)
import System.Posix.Files hiding (isDirectory)

-- | Create a configuration
makeConf :: HashAlgorithm h
         => [Int]
         -> Context h
         -> OutputDesc
         -> FilePath
         -> HashFSConf h
makeConf depth initCtx oDesc rootFs =
    HashFSConf { hashfsDepth      = depth
               , hashfsHash       = initCtx
               , hashfsOutputDesc = oDesc
               , hashfsRoot       = rootFs
               }

-- | Create a configuration with SHA256 as the Hash
makeConfSHA512 :: [Int] -> OutputDesc -> FilePath -> HashFSConf SHA512
makeConfSHA512 depth = makeConf depth hashInit

-- | Compute the digest according to the HashFSConof of any file
computeHash :: HashAlgorithm h => FilePath -> HashFS h (Digest h)
computeHash file =
    ask >>= \conf ->
    hashFinalize . hashUpdates (hashfsHash conf) . L.toChunks <$> liftIO (L.readFile file)

-- | make sure the hierarchy required to put the file is created
createHier :: HashAlgorithm h => Digest h -> HashFS h ()
createHier hsh = do
    conf <- ask
    let spath = getDigestSplitPath conf hsh
    -- FIXME do it recursively, as it depends on the configured depth
    destPath <- getPath hsh
    when (length spath > 1) $ liftIO $ createDirectoryIfMissing True (dropFileName $ dropTrailingPathSeparator destPath)

-- | Copy a file to the necessary digest
copyFileToHash :: HashAlgorithm h
               => ((Context h -> ByteString -> IO (Context h)) -> Context h -> IO (Context h))
               -> HashFS h (Digest h)
copyFileToHash blocksAcc = do
    conf <- ask
    (tmpFile, tmpHandle) <- tmpfilePath
    hsh  <- liftIO $ hashFinalize <$> blocksAcc (copyAndHash tmpHandle) (hashfsHash conf)
    liftIO $ hClose tmpHandle
    existsAlready <- exists hsh
    if existsAlready
        then liftIO (removeFile tmpFile) >> return hsh
        else do
            createHier hsh
            destPath <- getPath hsh
            liftIO $ rename tmpFile destPath
            return hsh
  where copyAndHash outHandle hashCtx block =
            B.hPut outHandle block >> return (hashUpdate hashCtx block)

-- | initialize ! :)
initialize :: HashAlgorithm h => HashFS h ()
initialize = do
    c     <- hashfsRoot <$> ask
    rootExists <- liftIO $ doesDirectoryExist c
    unless rootExists $ error "hashfs root directory doesn't exist"
    isNull <- null . filter metaDirs <$> liftIO (getDirectoryContents c)
    unless isNull $ error "hashfs root directory isn't empty"
    return ()
  where metaDirs = not . flip elem [".", ".."]

data ImportType = ImportCopy | ImportSymlink | ImportHardlink
    deriving (Show,Eq)

-- | import a file into a Hash Filesystem, and return the new digest
importFile :: HashAlgorithm h => ImportType -> FilePath -> HashFS h (Digest h)
importFile itype file = do
    case itype of
        ImportCopy     -> do
            c <- liftIO $ L.readFile file
            copyFileToHash (\f ctx -> foldM f ctx $ L.toChunks c)
        ImportSymlink  -> doImportLink True
        ImportHardlink -> doImportLink False
  where doImportLink isSymlink = do
            digest   <- computeHash file
            destPath <- getPath digest
            createHier digest
            liftIO $ if isSymlink
                then createSymbolicLink file destPath
                else createLink file destPath
            return digest

-- | import a file to a specified Digest value. use with care.
importFileAt :: HashAlgorithm h => FilePath -> Digest h -> HashFS h ()
importFileAt file digest = do
    existsAlready <- exists digest
    fp <- getPath digest
    (tmpFile, tmpHandle) <- tmpfilePath
    liftIO $ hClose tmpHandle
    if existsAlready
        then return ()
        else liftIO (L.readFile file >>= L.writeFile tmpFile >> rename tmpFile fp)

-- | Delete a file from the hash filesystem
deleteFile :: HashAlgorithm h => Digest h -> HashFS h ()
deleteFile digest = getPath digest >>= liftIO . removeFile

-- | Verify that a specific digest is backed by
-- a valid file where the hash exists
verify :: HashAlgorithm h => Digest h -> HashFS h Bool
verify digest = do
    conf      <- ask
    digestGot <- onDigestFile digest (liftIO . catchIO . (\f -> doHash conf <$> L.readFile f))
    return $ Just digest == digestGot
  where doHash conf = hashFinalize . hashUpdates (hashfsHash conf) . L.toChunks

-- | compute something on a filepath associated with a digest
onDigestFile :: HashAlgorithm h => Digest h -> (FilePath -> HashFS h a) -> HashFS h a
onDigestFile digest f = getPath digest >>= f

-- | Check that a digest exists in the filesystem
exists :: HashAlgorithm h => Digest h -> HashFS h Bool
exists digest = onDigestFile digest (liftIO . doesFileExist)

-- | try to read a file backed by a certain digest
readFile :: HashAlgorithm h => Digest h -> HashFS h (Maybe L.ByteString)
readFile digest = onDigestFile digest (liftIO . catchIO . L.readFile)

-- | get information about a specific Digest, namely size and mtime
readInfo :: HashAlgorithm h => Digest h -> HashFS h (Maybe (Word64, POSIXTime))
readInfo digest = onDigestFile digest (\path -> liftIO $ catchIO (toInfo <$> getFileStatus path))
  where toInfo fstat = (fromIntegral $ fileSize fstat, realToFrac $ modificationTime fstat)

-- | Take a list of prefixes and returns chunks in this prefix path directory
iterateFiles :: HashAlgorithm h => (Digest h -> IO ()) -> HashFS h ()
iterateFiles callback =
    ask >>= \conf ->
    loop (maybe (return ()) callback . inputd conf) (hashfsDepth conf) (fullLength conf) "" (hashfsRoot conf)
  where loop :: HashAlgorithm h => (String -> IO ()) -> [Int] -> Int -> String -> FilePath -> HashFS h ()
        loop f (n:ns) i pre dir = do
            ents <- liftIO $ getDirectoryContents dir
            let validEnts = filter ((== n) . length) ents
            validEntsDir <- filterM (liftIO . doesDirectoryExist . (dir </>)) validEnts
            mapM_ (\p -> loop f ns i (pre ++ p) (dir </> p)) validEntsDir
        loop f []     _ pre dir = do
            ents <- liftIO $ getDirectoryContents dir
            forM_ ents $ \r -> liftIO $ f (pre ++ r)

        fullLength :: HashAlgorithm h => HashFSConf h -> Int
        fullLength conf = length $ outputDigest (hashfsOutputDesc conf) $ hashFinalize (hashfsHash conf)

        --inputd :: HashAlgorithm h => HashFSConf h -> String -> Maybe (Digest h)
        inputd conf s = inputDigest (hashfsOutputDesc conf) s
