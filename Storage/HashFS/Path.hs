-- routines for path manipulation
module Storage.HashFS.Path
    ( Prefix(..)
    , getDigestSplitPath
    , getPathInRepo
    , getPath
    , tmpfilePath
    ) where

import Control.Monad.Reader (ask)
import Control.Monad.Trans
import Storage.HashFS.Types
import Storage.HashFS.Monad
import System.IO (Handle, openBinaryTempFile)
import Crypto.Hash
import System.FilePath (joinPath, (</>))
import Data.String

-- | Represent a chunk of digest
newtype Prefix = Prefix String

instance IsString Prefix where
    fromString s = Prefix s

-- | Get the chunks of path according to the Depth and ASCII configuration of HashFS and 
--
-- for example the ascii digest 1ec3124 with a depth of [3,1] will return:
--    ["1ec","3","124"]
getDigestSplitPath :: HashAlgorithm h => HashFSConf h -> Digest h -> [FilePath]
getDigestSplitPath conf digest = splitDepth (hashfsDepth conf) out
  where out = outputDigest (hashfsOutputDesc conf) digest
        splitDepth [] s | null s    = []
                        | otherwise = [s]
        splitDepth (x:xs) s
            | length s < x = error "depth function and digest are incompatible"
            | otherwise    =
                let (f1,f2) = splitAt x s
                 in f1 : splitDepth xs f2

-- | Return the path of a digest relative to the root of the hash filesystem
getPathInRepo :: HashAlgorithm h => Digest h -> HashFS h FilePath
getPathInRepo digest = ask >>= \conf -> return $ joinPath $ getDigestSplitPath conf digest

-- | Return an absolute path of the digest
getPath :: HashAlgorithm h => Digest h -> HashFS h FilePath
getPath digest = ask >>= \conf -> return (hashfsRoot conf </> (joinPath $ getDigestSplitPath conf digest))

-- | get a temporary file. the file is not deleted after use.
tmpfilePath :: HashAlgorithm h => HashFS h (FilePath, Handle)
tmpfilePath = ask >>= \conf -> liftIO (openBinaryTempFile (hashfsRoot conf) ".tmp")
