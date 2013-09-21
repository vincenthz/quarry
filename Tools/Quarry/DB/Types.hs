{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tools.Quarry.DB.Types
    ( HasTable(..)
    , PrimaryKey(..)
    , Keyable(..)
    , Table(..)
    , tableData
    , tableTag
    , tableCategory
    , KeyData(..)
    , KeyTag(..)
    , KeyCategory(..)
    ) where

import Data.Convertible
import Data.Data
import Database.HDBC

newtype Table = Table { tableName :: String }
    deriving (Show,Eq)

tableData, tableTag, tableCategory :: Table
tableData     = Table "data"
tableTag      = Table "tag"
tableCategory = Table "category"

-- | belong to a table
class HasTable a where
    getTable :: a -> Table

-- | A primary key type from a specific table
class HasTable a => PrimaryKey a where
    getPrimaryKey :: a -> Integer

-- | Anything that can be primary keyed
class (HasTable a, PrimaryKey key) => Keyable a key where
    getKey :: IConnection con => con -> a -> IO (Maybe key)

newtype KeyData     = KeyData Integer
    deriving (Show,Eq,Ord,Data,Typeable)
newtype KeyTag      = KeyTag Integer
    deriving (Show,Eq,Ord,Data,Typeable)
newtype KeyCategory = KeyCategory Integer
    deriving (Show,Eq,Ord,Data,Typeable)

instance HasTable KeyData where
    getTable _ = tableData
instance HasTable KeyTag where
    getTable _ = tableTag
instance HasTable KeyCategory where
    getTable _ = tableCategory
instance PrimaryKey KeyData where
    getPrimaryKey (KeyData k) = k
instance PrimaryKey KeyTag where
    getPrimaryKey (KeyTag k) = k
instance PrimaryKey KeyCategory where
    getPrimaryKey (KeyCategory k) = k
instance Convertible SqlValue KeyData where
    safeConvert (SqlInteger i) = return $ KeyData i
    safeConvert y              = convError "incompatible types" y

