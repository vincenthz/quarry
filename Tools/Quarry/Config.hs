module Tools.Quarry.Config
    ( QuarryDB
    , QuarryConfig(..)
    ) where

import Database.HDBC.Sqlite3 (Connection)
import Tools.Quarry.Types
import Tools.Quarry.DB.Types
import Tools.Quarry.Cache
import Storage.HashFS (HashFSConf)
import Crypto.Hash (SHA512)

-- | Config
data QuarryConfig = QuarryConfig
    { connection :: Connection
    , hashfsConf :: HashFSConf SHA512
    , cacheTags  :: CacheTable KeyTag Tag
    }

type QuarryDB = Connection
