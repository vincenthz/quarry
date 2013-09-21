module Tools.Quarry.Types where

import Crypto.Hash (SHA512, Digest)

type TagName = String
type Category = String

data Tag = Tag { tagName :: TagName, tagCat :: Category }
    deriving (Eq)

instance Show Tag where
    show tag = tagCat tag ++ ":" ++ tagName tag

type QuarryDigest = Digest SHA512

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
