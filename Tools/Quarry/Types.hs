module Tools.Quarry.Types where

import Database.HDBC.Sqlite3 (Connection)
import Crypto.Hash (SHA512, Digest)
import Storage.HashFS (HashFSConf)

type QuarryDB = Connection
type Tag = String
type QuarryDigest = Digest SHA512

-- | Config
data QuarryConfig = QuarryConfig
    { connection :: Connection
    , hashfsConf :: HashFSConf SHA512
    }

data QuarryFileType =
      QuarryTypeImage
    | QuarryTypeVideo
    | QuarryTypeSound
    | QuarryTypeDocument
    | QuarryTypeUnknown
    deriving (Eq, Ord, Enum)

instance Show QuarryFileType where
    show QuarryTypeUnknown  = "unknown"
    show QuarryTypeImage    = "image"
    show QuarryTypeVideo    = "video"
    show QuarryTypeSound    = "sound"
    show QuarryTypeDocument = "document"

instance Read QuarryFileType where
    readsPrec _ value = maybe (error "mediatype") (\v -> [(v,"")]) $ lookup value
        [ ("img"      , QuarryTypeImage)
        , ("image"    , QuarryTypeImage)
        , ("snd"      , QuarryTypeSound)
        , ("sound"    , QuarryTypeSound)
        , ("vid"      , QuarryTypeVideo)
        , ("video"    , QuarryTypeVideo)
        , ("doc"      , QuarryTypeDocument)
        , ("txt"      , QuarryTypeDocument)
        , ("text"     , QuarryTypeDocument)
        , ("document" , QuarryTypeDocument)
        ]

{-
validateFiletype "img"      = QuarryTypeImage
validateFiletype "image"    = QuarryTypeImage
validateFiletype "snd"      = QuarryTypeSound
validateFiletype "sound"    = QuarryTypeSound
validateFiletype "vid"      = QuarryTypeVideo
validateFiletype "video"    = QuarryTypeVideo
validateFiletype "doc"      = QuarryTypeDocument
validateFiletype "txt"      = QuarryTypeDocument
validateFiletype "text"     = QuarryTypeDocument
validateFiletype "document" = QuarryTypeDocument
validateFiletype s          = error ("unrecognized format type : " ++ show s)
-}
