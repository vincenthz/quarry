{-# LANGUAGE OverloadedStrings #-}
module Storage.HashFS.Types where

import Crypto.Hash
import Data.Byteable
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BC

-- | Configuration for HashFS
data HashAlgorithm h => HashFSConf h = HashFSConf
    {
    -- | specify how to split the hash into directory and file.
    -- e.g. [1,2] will split into a path a/bc/res
      hashfsDepth :: [Int]
    -- | the hash function used
    , hashfsHash  :: Context h
    -- | The ascii function used
    , hashfsOutputDesc :: OutputDesc
    -- | Root of the hash filesystem
    , hashfsRoot :: FilePath
    }

-- | Internal variant to handle output format of ASCII
data OutputFormat = OutputString String | OutputByteString ByteString

lengthOutputFormat :: OutputFormat -> Int
lengthOutputFormat (OutputString s) = length s
lengthOutputFormat (OutputByteString s) = B.length s

-- | type of encoding for digest output
data OutputDesc =
      OutputHex    -- ^ hexadecimal, base16 output
    | OutputBase64 -- ^ base64 with / as _. doesn't work on CI filesystem.
    -- | OutputCustom (Char -> Bool) (Digest h -> OutputFormat) (OutputFormat -> Digest h)
    deriving (Show,Read,Eq)

validChar :: OutputDesc -> Char -> Bool
validChar OutputHex c
    | fromEnum c < 0x80 = maybe False (const True) $ B.elemIndex (fromIntegral $ fromEnum c) base16Dict
    | otherwise         = False
validChar OutputBase64 c
    | fromEnum c < 0x80 = maybe False (const True) $ B.elemIndex (fromIntegral $ fromEnum c) base64Dict
    | otherwise         = False

-- | Transform a digest into a String
outputDigest :: OutputDesc -> Digest h -> String
outputDigest OutputHex digest    = show digest
outputDigest OutputBase64 digest = recode $ BC.unpack $ Base64.encode $ toBytes digest
  where recode []           = []
        recode ('=':[])     = []
        recode ('=':'=':[]) = []
        recode (x:xs) | x == '/'  = '_':recode xs
                      | otherwise = x:recode xs

-- | input a string and try to transform to a digest
inputDigest :: HashAlgorithm h => OutputDesc -> String -> Maybe (Digest h)
inputDigest OutputHex s
    | len == 0 || odd len = Nothing
    | and (map (validChar OutputHex) s) == False = Nothing
    | otherwise           = digestFromByteString $ B.pack $ loop s
  where len = length s
        loop []      = []
        loop (_:[])  = error "impossible"
        loop (x:y:z) = unhex x * 16 + unhex y : loop z
        unhex c | c >= '0' && c <= '9' = fromIntegral (fromEnum c - fromEnum '0')
                | c >= 'a' && c <= 'f' = fromIntegral (fromEnum c - fromEnum 'a' + 10)
                | otherwise            = error "invalid character"
inputDigest OutputBase64 s
    | len == 0  = Nothing
    | otherwise = Nothing
  where len = length s

base16Dict :: ByteString
base16Dict = "0123456789abcdef"

base64Dict :: ByteString
base64Dict = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+_"

iteratePathRoot :: Int -> OutputDesc -> [FilePath]
iteratePathRoot n OutputHex =
    case n of
        1 -> [ [a]   | a <- base ]
        2 -> [ [a,b] | a <- base, b <- base ]
        _ -> error "not implemented"
  where base = "0123456789abcdef"
iteratePathRoot n OutputBase64 =
    case n of
        1 -> [ [a]   | a <- base ]
        2 -> [ [a,b] | a <- base, b <- base ]
        _ -> error "not implemented"
  where base = BC.unpack base64Dict
